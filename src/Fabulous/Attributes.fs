namespace Fabulous

open System
open System.Runtime.CompilerServices
open Fabulous.ScalarAttributeDefinitions
open Fabulous.WidgetAttributeDefinitions
open Fabulous.WidgetCollectionAttributeDefinitions

module Helpers =
    let canReuse<'T when 'T: equality> (prev: 'T) (curr: 'T) = prev = curr

    let createViewForWidget (parent: IViewNode) (widget: inref<Widget>) =
        let widgetDefinition = WidgetDefinitionStore.get widget.Key
        widgetDefinition.CreateView(widget, parent.TreeContext, ValueSome parent)

module ScalarAttributeComparers =
    let inline noCompare _ _ = ScalarAttributeComparison.Different

    let inline equalityCompare (a: 'T) (b: 'T) =
        // F# (=) on a generic 'T compiles to FSharp.Core's comparer machinery, instantiated per value type under
        // NativeAOT. Value types get the BCL comparer instead; reference types keep (=), whose shared code also
        // compares arrays element-wise at runtime (classes and item sources are often freshly built arrays).
        if
            (if typeof<'T>.IsValueType then
                 System.Collections.Generic.EqualityComparer<'T>.Default.Equals(a, b)
             else
                 a = b)
        then
            ScalarAttributeComparison.Identical
        else
            ScalarAttributeComparison.Different

module SmallScalars =
    module Bool =
        let inline encode (v: bool) : uint64 = if v then 1UL else 0UL
        let inline decode (encoded: uint64) : bool = encoded = 1UL

    module Float =
        let inline encode (v: float) : uint64 =
            BitConverter.DoubleToInt64Bits v |> uint64

        let inline decode (encoded: uint64) : float =
            encoded |> int64 |> BitConverter.Int64BitsToDouble

    module Float32 =
        let inline encode (v: float32) : uint64 =
            v |> float |> BitConverter.DoubleToInt64Bits |> uint64

        let inline decode (encoded: uint64) : float32 =
            encoded |> int64 |> BitConverter.Int64BitsToDouble |> float32

    // TODO is there a better conversion algorithm?
    module Int =
        let inline encode (v: int) : uint64 = uint64 v

        let inline decode (encoded: uint64) : int = int encoded

    module UInt =
        let inline encode (v: uint) : uint64 = uint64 v

        let inline decode (encoded: uint64) : uint = uint encoded

    module IntEnum =
        let inline encode< ^T when ^T: enum<int> and ^T: (static member op_Explicit: ^T -> uint64)> (v: ^T) : uint64 = uint64 v

        let inline decode< ^T when ^T: enum<int>> (encoded: uint64) : ^T = enum< ^T>(int encoded)

[<Extension>]
type SmallScalarExtensions() =
    [<Extension>]
    static member inline WithValue(this: SmallScalarAttributeDefinition<bool>, value) =
        this.WithValue(value, SmallScalars.Bool.encode)

    [<Extension>]
    static member inline WithValue(this: SmallScalarAttributeDefinition<float>, value) =
        this.WithValue(value, SmallScalars.Float.encode)

    [<Extension>]
    static member inline WithValue(this: SmallScalarAttributeDefinition<float32>, value) =
        this.WithValue(value, SmallScalars.Float32.encode)

    [<Extension>]
    static member inline WithValue(this: SmallScalarAttributeDefinition<int>, value) =
        this.WithValue(value, SmallScalars.Int.encode)

    [<Extension>]
    static member inline WithValue< ^T when ^T: enum<int> and ^T: (static member op_Explicit: ^T -> uint64)>
        (this: SmallScalarAttributeDefinition< ^T >, value)
        =
        this.WithValue(value, SmallScalars.IntEnum.encode)

type MsgValue = MsgValue of obj

[<Extension>]
type SimpleScalarAttributeDefinitionExtensions() =
    [<Extension>]
    static member inline WithValue(this: SimpleScalarAttributeDefinition<'args -> MsgValue>, value: 'args -> 'msg) =
        this.WithValue(value >> box >> Unchecked.nonNull >> MsgValue)

/// The node's registration for one event attribute: subscribed to the event once, forwarding to whatever handler the
/// latest render supplied. Re-rendering only swaps Handler, instead of disposing and re-subscribing every time.
[<Sealed; AllowNullLiteral>]
type EventHandlerSlot(handler: obj) =
    member val Handler = handler with get, set

    /// Set while the attribute itself changes the target, so the change it causes is not reported back as an event
    member val Suppressed = false with get, set

    member val Subscription: IDisposable | null = null with get, set

    interface IDisposable with
        member this.Dispose() =
            match this.Subscription with
            | null -> ()
            | subscription ->
                this.Subscription <- null
                subscription.Dispose()

module EventHandlerSlot =
    /// The slot registered under key, if the node already has one
    let tryGet (node: IViewNode) (key: string) =
        match node.TryGetHandler(key) with
        | :? EventHandlerSlot as slot -> slot
        | _ -> null

    /// Points the node's slot at handler, subscribing only when there is no slot yet
    let inline set (node: IViewNode) (key: string) (handler: obj) ([<InlineIfLambda>] subscribe: EventHandlerSlot -> IDisposable) =
        match tryGet node key with
        | null ->
            match node.TryGetHandler(key) with
            | null -> ()
            | other -> other.Dispose()

            let slot = new EventHandlerSlot(handler)
            slot.Subscription <- subscribe slot
            node.SetHandler(key, slot)
        | slot -> slot.Handler <- handler

/// Event attributes never compare equal (handlers are fresh closures on every render), so they are always re-applied
[<AbstractClass>]
type EventAttributeData(name: string) =
    inherit ScalarAttributeData()

    /// Subscribes to the event, invoking slot.Handler (read on every event, not captured) when it fires
    abstract Subscribe: slot: EventHandlerSlot * node: IViewNode -> IDisposable

    override _.CompareBoxed(_, _) = ScalarAttributeComparison.Different

    override this.UpdateNode(_, newValue, node) =
        match newValue with
        | ValueSome handler -> EventHandlerSlot.set node name handler (fun slot -> this.Subscribe(slot, node))
        | ValueNone -> node.RemoveHandler(name)

[<Sealed>]
type MvuEventNoArgData(name: string, getEvent: obj -> IEvent<EventHandler, EventArgs>) =
    inherit EventAttributeData(name)

    override _.Subscribe(slot, node) =
        (getEvent node.Target)
            .Subscribe(fun _ ->
                let (MsgValue msg) = unbox<MsgValue> slot.Handler
                Dispatcher.dispatch node msg)

[<Sealed>]
type MvuEventData<'args>(name: string, getEvent: obj -> IEvent<EventHandler<'args>, 'args>) =
    inherit EventAttributeData(name)

    override _.Subscribe(slot, node) =
        (getEvent node.Target)
            .Subscribe(fun args ->
                let (MsgValue r) = (unbox<'args -> MsgValue> slot.Handler) args
                Dispatcher.dispatch node r)

[<Sealed>]
type ComponentEventNoArgData(name: string, getEvent: obj -> IEvent<EventHandler, EventArgs>) =
    inherit EventAttributeData(name)

    override _.Subscribe(slot, node) =
        (getEvent node.Target).Subscribe(fun _ -> (unbox<unit -> unit> slot.Handler) ())

[<Sealed>]
type ComponentEventData<'args>(name: string, getEvent: obj -> IEvent<EventHandler<'args>, 'args>) =
    inherit EventAttributeData(name)

    override _.Subscribe(slot, node) =
        (getEvent node.Target).Subscribe(fun args -> (unbox<'args -> unit> slot.Handler) args)

module Attributes =
    /// Define an attribute that can fit into 8 bytes encoded as uint64 (such as float or bool)
    let inline defineSmallScalar<'T>
        name
        ([<InlineIfLambda>] decode: uint64 -> 'T)
        ([<InlineIfLambda>] updateNode: ScalarValue<'T> -> ScalarValue<'T> -> IViewNode -> unit)
        : SmallScalarAttributeDefinition<'T> =
        let key =
            SmallScalarAttributeDefinition.CreateAttributeData<'T>(decode, updateNode)
            |> AttributeDefinitionStore.registerSmallScalar

        { Key = key; Name = name }

    /// Define an attribute that can store any value with no conversion.
    /// The value will be boxed and allocated on the heap.
    /// For better performance, use defineSmallScalar instead.
    let inline defineSimpleScalar<'T>
        name
        ([<InlineIfLambda>] compare: 'T -> 'T -> ScalarAttributeComparison)
        ([<InlineIfLambda>] updateNode: ScalarValue<'T> -> ScalarValue<'T> -> IViewNode -> unit)
        : SimpleScalarAttributeDefinition<'T> =
        let key =
            SimpleScalarAttributeDefinition.CreateAttributeData(compare, updateNode)
            |> AttributeDefinitionStore.registerScalar

        { Key = key; Name = name }

    /// Define an attribute that can store any value with a conversion.
    /// The value will be boxed and allocated on the heap.
    /// For better performance, use defineSmallScalar instead.
    let inline defineScalar<'modelType, 'valueType>
        name
        ([<InlineIfLambda>] convertValue: 'modelType -> 'valueType)
        ([<InlineIfLambda>] compare: 'modelType -> 'modelType -> ScalarAttributeComparison)
        ([<InlineIfLambda>] updateNode: ScalarValue<'valueType> -> ScalarValue<'valueType> -> IViewNode -> unit)
        : ScalarAttributeDefinition<'modelType, 'valueType> =
        let key =
            ScalarAttributeDefinition.CreateAttributeData<'modelType, 'valueType>(convertValue, compare, updateNode)
            |> AttributeDefinitionStore.registerScalar

        { Key = key; Name = name }

    /// Define an int attribute that is encoded into uint64
    let inline defineInt name ([<InlineIfLambda>] updateNode: ScalarValue<int> -> ScalarValue<int> -> IViewNode -> unit) : SmallScalarAttributeDefinition<int> =

        defineSmallScalar name SmallScalars.Int.decode updateNode

    /// Define a float attribute that is encoded into uint64
    let inline defineFloat name ([<InlineIfLambda>] updateNode: ScalarValue<float> -> ScalarValue<float> -> IViewNode -> unit) : SmallScalarAttributeDefinition<float> =

        defineSmallScalar name SmallScalars.Float.decode updateNode

    /// Define a enum attribute that is encoded into uint64
    let inline defineEnum< ^T when ^T: enum<int>>
        name
        ([<InlineIfLambda>] updateNode: ScalarValue< ^T > -> ScalarValue< ^T > -> IViewNode -> unit)
        : SmallScalarAttributeDefinition< ^T > =
        defineSmallScalar name SmallScalars.IntEnum.decode updateNode

    /// Define a boolean attribute that is encoded into uint64
    let inline defineBool name ([<InlineIfLambda>] updateNode: ScalarValue<bool> -> ScalarValue<bool> -> IViewNode -> unit) : SmallScalarAttributeDefinition<bool> =

        defineSmallScalar name SmallScalars.Bool.decode updateNode

    /// Define an attribute storing a single Widget.
    /// Used for storing the single child of a view
    let inline defineWidget
        name
        (applyDiff: WidgetDiff -> IViewNode -> unit)
        (updateNode: Widget voption -> Widget voption -> IViewNode -> unit)
        : WidgetAttributeDefinition =

        let key =
            AttributeDefinitionStore.registerWidget(WidgetAttributeFuncData(applyDiff, updateNode))

        { Key = key; Name = name }

    /// Define an attribute storing a collection of Widgets
    /// Used for storing children of a view
    let inline defineWidgetCollection
        name
        (applyDiff: ArraySlice<Widget> -> WidgetCollectionItemChanges -> IViewNode -> unit)
        (updateNode: ArraySlice<Widget> voption -> ArraySlice<Widget> voption -> IViewNode -> unit)
        : WidgetCollectionAttributeDefinition =

        let key =
            AttributeDefinitionStore.registerWidgetCollection(WidgetCollectionAttributeFuncData(applyDiff, updateNode))

        { Key = key; Name = name }

    /// Define an attribute storing a Widget for a CLR property
    let definePropertyWidget<'T when 'T: null> (name: string) (get: obj -> obj) (set: obj -> 'T -> unit) =
        let applyDiff (diff: WidgetDiff) (node: IViewNode) =
            let childView = get node.Target

            let childNode = node.TreeContext.GetViewNode(childView)

            childNode.ApplyDiff(&diff)

        let updateNode (oldValueOpt: Widget voption) (newValueOpt: Widget voption) (node: IViewNode) =
            if oldValueOpt.IsSome then
                // Dispose the existing child if it exists
                match get node.Target with
                | null -> ()
                | childView ->
                    let childNode = node.TreeContext.GetViewNode(childView)
                    childNode.Dispose()

            match newValueOpt with
            | ValueNone -> set node.Target null
            | ValueSome widget ->
                let struct (_, view) = Helpers.createViewForWidget node &widget
                set node.Target (unbox view)

        defineWidget name applyDiff updateNode

    /// Define an attribute storing a collection of Widget for a List<T> property
    let defineListWidgetCollection<'itemType> name (getCollection: obj -> System.Collections.Generic.IList<'itemType>) =
        let applyDiff _ (diffs: WidgetCollectionItemChanges) (node: IViewNode) =
            let targetColl = getCollection node.Target

            for diff in diffs do
                match diff with
                | WidgetCollectionItemChange.Remove(index, widget) ->
                    let itemNode = node.TreeContext.GetViewNode(box targetColl[index])

                    // Trigger the unmounted event
                    Dispatcher.dispatchEventForAllChildren itemNode &widget Lifecycle.Unmounted
                    itemNode.Dispose()

                    // Remove the child from the UI tree
                    targetColl.RemoveAt(index)

                | _ -> ()

            for diff in diffs do
                match diff with
                | WidgetCollectionItemChange.Insert(index, widget) ->
                    let struct (itemNode, view) = Helpers.createViewForWidget node &widget

                    // Insert the new child into the UI tree
                    targetColl.Insert(index, unbox view)

                    // Trigger the mounted event
                    Dispatcher.dispatchEventForAllChildren itemNode &widget Lifecycle.Mounted

                | WidgetCollectionItemChange.Update(index, widgetDiff) ->
                    let childNode = node.TreeContext.GetViewNode(box targetColl[index])

                    childNode.ApplyDiff(&widgetDiff)

                | WidgetCollectionItemChange.Replace(index, oldWidget, newWidget) ->
                    let prevItemNode = node.TreeContext.GetViewNode(box targetColl[index])

                    let struct (nextItemNode, view) = Helpers.createViewForWidget node &newWidget

                    // Trigger the unmounted event for the old child
                    Dispatcher.dispatchEventForAllChildren prevItemNode &oldWidget Lifecycle.Unmounted
                    prevItemNode.Dispose()

                    // Replace the existing child in the UI tree at the index with the new one
                    targetColl[index] <- unbox view

                    // Trigger the mounted event for the new child
                    Dispatcher.dispatchEventForAllChildren nextItemNode &newWidget Lifecycle.Mounted

                | _ -> ()

        let updateNode (oldValueOpt: ArraySlice<Widget> voption) (newValueOpt: ArraySlice<Widget> voption) (node: IViewNode) =
            let targetColl = getCollection node.Target

            match oldValueOpt with
            | ValueNone -> ()
            | ValueSome oldValue ->
                // Dispose the existing children if they exist
                for index = 0 to ArraySlice.length oldValue - 1 do
                    match box targetColl[index] with
                    | null -> ()
                    | childView ->
                        let childNode = node.TreeContext.GetViewNode(childView)
                        childNode.Dispose()

            targetColl.Clear()

            match newValueOpt with
            | ValueNone -> ()
            | ValueSome widgets ->
                for widget in ArraySlice.toSpan widgets do
                    let struct (_, view) = Helpers.createViewForWidget node &widget

                    targetColl.Add(unbox view)

        defineWidgetCollection name applyDiff updateNode

    /// Define an attribute for a value supporting equality comparison
    let inline defineSimpleScalarWithEquality<'T when 'T: equality>
        name
        ([<InlineIfLambda>] updateTarget: ScalarValue<'T> -> ScalarValue<'T> -> IViewNode -> unit)
        : SimpleScalarAttributeDefinition<'T> =
        let key =
            SimpleScalarAttributeDefinition.CreateAttributeData(ScalarAttributeComparers.equalityCompare, updateTarget)
            |> AttributeDefinitionStore.registerScalar

        { Key = key; Name = name }

    module Mvu =
        /// Define an attribute for EventHandler
        let defineEventNoArg name (getEvent: obj -> IEvent<EventHandler, EventArgs>) : SimpleScalarAttributeDefinition<MsgValue> =
            { Key = AttributeDefinitionStore.registerScalar(MvuEventNoArgData(name, getEvent))
              Name = name }

        /// Define an attribute for EventHandler<'T>
        let defineEvent<'args> name (getEvent: obj -> IEvent<EventHandler<'args>, 'args>) : SimpleScalarAttributeDefinition<'args -> MsgValue> =
            { Key = AttributeDefinitionStore.registerScalar(MvuEventData<'args>(name, getEvent))
              Name = name }

    module Component =
        let defineEventNoArg name (getEvent: obj -> IEvent<EventHandler, EventArgs>) : SimpleScalarAttributeDefinition<unit -> unit> =
            { Key = AttributeDefinitionStore.registerScalar(ComponentEventNoArgData(name, getEvent))
              Name = name }

        let defineEvent<'args> name (getEvent: obj -> IEvent<EventHandler<'args>, 'args>) : SimpleScalarAttributeDefinition<'args -> unit> =
            { Key = AttributeDefinitionStore.registerScalar(ComponentEventData<'args>(name, getEvent))
              Name = name }
