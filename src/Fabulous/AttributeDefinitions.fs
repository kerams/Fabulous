namespace Fabulous

/// An optional scalar attribute value handed to attribute updaters.
/// Deliberately a plain struct and not 'T voption: F# unions carry [DynamicDependency] on their constructors,
/// which makes NativeAOT keep a boxed, reflection-invokable copy of Equals/CompareTo/GetHashCode/ToString
/// (plus the FSharp.Core comparers they use) for every 'T an attribute is defined over.
[<Struct; NoEquality; NoComparison>]
type ScalarValue<'T> =
    val HasValue: bool
    val Value: 'T
    new(value: 'T) = { HasValue = true; Value = value }

// Attribute data are objects with virtual methods rather than records of closures: a closure record costs two wrapper
// closure types plus the curried FSharpFunc/OptimizedClosures instantiations for every value type an attribute is
// defined over under NativeAOT, and each call goes through InvokeFast. Frequently instantiated definitions get a
// dedicated sealed subclass; the *FuncData classes adapt the function-based APIs.
module ScalarAttributeDefinitions =
    /// A small scalar attribute.
    /// When we can encode the value as a uint64 (64 bits), we should prefer this type.
    /// The value will be kept on the stack avoiding GC pressure.
    [<AbstractClass>]
    type SmallScalarAttributeData() =
        abstract UpdateNode: oldValue: uint64 voption * newValue: uint64 voption * node: IViewNode -> unit

    /// A regular scalar attribute.
    /// The value will be boxed and put on the heap, which can trigger GC to pass.
    /// Prefer small scalar attribute when possible.
    [<AbstractClass>]
    type ScalarAttributeData() =
        abstract CompareBoxed: a: obj * b: obj -> ScalarAttributeComparison
        abstract UpdateNode: oldValue: obj voption * newValue: obj voption * node: IViewNode -> unit

    [<Sealed>]
    type SmallScalarAttributeFuncData<'T>(decode: uint64 -> 'T, updateNode: ScalarValue<'T> -> ScalarValue<'T> -> IViewNode -> unit) =
        inherit SmallScalarAttributeData()

        override _.UpdateNode(oldValueOpt, newValueOpt, node) =
            let oldValue =
                match oldValueOpt with
                | ValueNone -> Unchecked.defaultof<_>
                | ValueSome v -> ScalarValue(decode v)

            let newValue =
                match newValueOpt with
                | ValueNone -> Unchecked.defaultof<_>
                | ValueSome v -> ScalarValue(decode v)

            updateNode oldValue newValue node

    [<Sealed>]
    type SimpleScalarAttributeFuncData<'T>
        (compare: 'T -> 'T -> ScalarAttributeComparison, updateNode: ScalarValue<'T> -> ScalarValue<'T> -> IViewNode -> unit) =
        inherit ScalarAttributeData()

        override _.CompareBoxed(a, b) = compare (unbox<'T> a) (unbox<'T> b)

        override _.UpdateNode(oldValueOpt, newValueOpt, node) =
            let oldValue =
                match oldValueOpt with
                | ValueNone -> Unchecked.defaultof<_>
                | ValueSome v -> ScalarValue(unbox<'T> v)

            let newValue =
                match newValueOpt with
                | ValueNone -> Unchecked.defaultof<_>
                | ValueSome v -> ScalarValue(unbox<'T> v)

            updateNode oldValue newValue node

    [<Sealed>]
    type ScalarAttributeFuncData<'modelType, 'valueType>
        (
            convertValue: 'modelType -> 'valueType,
            compare: 'modelType -> 'modelType -> ScalarAttributeComparison,
            updateNode: ScalarValue<'valueType> -> ScalarValue<'valueType> -> IViewNode -> unit
        ) =
        inherit ScalarAttributeData()

        override _.CompareBoxed(a, b) =
            compare (unbox<'modelType> a) (unbox<'modelType> b)

        override _.UpdateNode(oldValueOpt, newValueOpt, node) =
            let oldValue =
                match oldValueOpt with
                | ValueNone -> Unchecked.defaultof<_>
                | ValueSome v -> ScalarValue(convertValue(unbox<'modelType> v))

            let newValue =
                match newValueOpt with
                | ValueNone -> Unchecked.defaultof<_>
                | ValueSome v -> ScalarValue(convertValue(unbox<'modelType> v))

            updateNode oldValue newValue node

    /// Attribute definition for small scalar properties (encodable on 64 bits)
    [<Struct; NoEquality; NoComparison>]
    type SmallScalarAttributeDefinition<'T> =
        { Key: ScalarAttributeKey
          Name: string }

        member inline x.WithValue(value: 'T, [<InlineIfLambda>] encode: 'T -> uint64) : ScalarAttribute =
            { Key = x.Key
#if DEBUG
              DebugName = x.Name
#endif
              NumericValue = encode(value)
              Value = null }

        static member CreateAttributeData<'T>
            (decode: uint64 -> 'T, updateNode: ScalarValue<'T> -> ScalarValue<'T> -> IViewNode -> unit)
            : SmallScalarAttributeData =
            SmallScalarAttributeFuncData<'T>(decode, updateNode)

    /// Attribute definition for boxed scalar properties
    [<Struct; NoEquality; NoComparison>]
    type SimpleScalarAttributeDefinition<'T> =
        { Key: ScalarAttributeKey
          Name: string }

        member inline x.WithValue(value: 'T) : ScalarAttribute =
            { Key = x.Key
#if DEBUG
              DebugName = x.Name
#endif
              NumericValue = 0UL
              Value = value }

        static member CreateAttributeData
            (compare: 'T -> 'T -> ScalarAttributeComparison, updateNode: ScalarValue<'T> -> ScalarValue<'T> -> IViewNode -> unit)
            : ScalarAttributeData =
            SimpleScalarAttributeFuncData<'T>(compare, updateNode)

    /// Attribute definition for boxed scalar properties with a custom conversion before being applied to the view
    [<Struct; NoEquality; NoComparison>]
    type ScalarAttributeDefinition<'modelType, 'valueType> =
        { Key: ScalarAttributeKey
          Name: string }

        member inline x.WithValue(value: 'modelType) : ScalarAttribute =
            { Key = x.Key
#if DEBUG
              DebugName = x.Name
#endif
              NumericValue = 0UL
              Value = value }

        static member CreateAttributeData<'modelType, 'valueType>
            (
                convertValue: 'modelType -> 'valueType,
                compare: 'modelType -> 'modelType -> ScalarAttributeComparison,
                updateNode: ScalarValue<'valueType> -> ScalarValue<'valueType> -> IViewNode -> unit
            ) : ScalarAttributeData =
            ScalarAttributeFuncData<'modelType, 'valueType>(convertValue, compare, updateNode)

module WidgetAttributeDefinitions =
    [<AbstractClass>]
    type WidgetAttributeData() =
        abstract ApplyDiff: diff: WidgetDiff * node: IViewNode -> unit
        abstract UpdateNode: oldValue: Widget voption * newValue: Widget voption * node: IViewNode -> unit

    [<Sealed>]
    type WidgetAttributeFuncData
        (applyDiff: WidgetDiff -> IViewNode -> unit, updateNode: Widget voption -> Widget voption -> IViewNode -> unit) =
        inherit WidgetAttributeData()
        override _.ApplyDiff(diff, node) = applyDiff diff node
        override _.UpdateNode(oldValue, newValue, node) = updateNode oldValue newValue node

    /// Attribute definition for widget properties
    [<Struct; NoEquality; NoComparison>]
    type WidgetAttributeDefinition =
        { Key: WidgetAttributeKey
          Name: string }

        member inline x.WithValue(value: Widget) : WidgetAttribute =
            { Key = x.Key
#if DEBUG
              DebugName = x.Name
#endif
              Value = value }

module WidgetCollectionAttributeDefinitions =
    [<AbstractClass>]
    type WidgetCollectionAttributeData() =
        abstract ApplyDiff: oldValue: ArraySlice<Widget> * changes: WidgetCollectionItemChanges * node: IViewNode -> unit
        abstract UpdateNode: oldValue: ArraySlice<Widget> voption * newValue: ArraySlice<Widget> voption * node: IViewNode -> unit

    [<Sealed>]
    type WidgetCollectionAttributeFuncData
        (
            applyDiff: ArraySlice<Widget> -> WidgetCollectionItemChanges -> IViewNode -> unit,
            updateNode: ArraySlice<Widget> voption -> ArraySlice<Widget> voption -> IViewNode -> unit
        ) =
        inherit WidgetCollectionAttributeData()
        override _.ApplyDiff(oldValue, changes, node) = applyDiff oldValue changes node
        override _.UpdateNode(oldValue, newValue, node) = updateNode oldValue newValue node

    /// Attribute definition for collection properties
    [<Struct; NoEquality; NoComparison>]
    type WidgetCollectionAttributeDefinition =
        { Key: WidgetCollectionAttributeKey
          Name: string }

        member inline x.WithValue(value: ArraySlice<Widget>) : WidgetCollectionAttribute =
            { Key = x.Key
#if DEBUG
              DebugName = x.Name
#endif
              Value = value }

module AttributeDefinitionStore =
    open ScalarAttributeDefinitions
    open WidgetAttributeDefinitions
    open WidgetCollectionAttributeDefinitions

    let private _scalars = ResizeArray<ScalarAttributeData>()
    let private _smallScalars = ResizeArray<SmallScalarAttributeData>()
    let private _widgets = ResizeArray<WidgetAttributeData>()
    let private _widgetCollections = ResizeArray<WidgetCollectionAttributeData>()

    /// Guards registration. Definitions are created lazily on first use, and update functions can build widgets on
    /// thread pool threads while the UI thread renders. Lookups stay lock-free: a key is only handed out once its
    /// entry has been added.
    let SyncRoot = System.Threading.Lock()

    let registerSmallScalar (data: SmallScalarAttributeData) : ScalarAttributeKey =
        SyncRoot.Enter()

        try
            let index = _smallScalars.Count
            _smallScalars.Add(data)
            (index ||| ScalarAttributeKey.Code.Inline) * 1<scalarAttributeKey>
        finally
            SyncRoot.Exit()

    let registerScalar (data: ScalarAttributeData) : ScalarAttributeKey =
        SyncRoot.Enter()

        try
            let index = _scalars.Count
            _scalars.Add(data)
            (index ||| ScalarAttributeKey.Code.Boxed) * 1<scalarAttributeKey>
        finally
            SyncRoot.Exit()

    let registerWidget (data: WidgetAttributeData) : WidgetAttributeKey =
        SyncRoot.Enter()

        try
            let index = _widgets.Count
            _widgets.Add(data)
            index * 1<widgetAttributeKey>
        finally
            SyncRoot.Exit()

    let registerWidgetCollection (data: WidgetCollectionAttributeData) : WidgetCollectionAttributeKey =
        SyncRoot.Enter()

        try
            let index = _widgetCollections.Count
            _widgetCollections.Add(data)
            index * 1<widgetCollectionAttributeKey>
        finally
            SyncRoot.Exit()

    let getScalar (key: ScalarAttributeKey) : ScalarAttributeData =
        let index = ScalarAttributeKey.getKeyValue key
        _scalars[index]

    let getSmallScalar (key: ScalarAttributeKey) : SmallScalarAttributeData =
        let index = ScalarAttributeKey.getKeyValue key
        _smallScalars[index]

    let getWidget (key: WidgetAttributeKey) : WidgetAttributeData = _widgets[int key]

    let getWidgetCollection (key: WidgetCollectionAttributeKey) : WidgetCollectionAttributeData = _widgetCollections[int key]

module AttributeHelpers =
    open ScalarAttributeDefinitions

    let tryFindSimpleScalarAttribute (definition: SimpleScalarAttributeDefinition<'T>) (widget: inref<Widget>) =
        let key = definition.Key
        match widget.ScalarAttributes |> Array.tryFind(fun attr -> attr.Key = key) with
        | None -> ValueNone
        | Some attr -> ValueSome(unbox<'T> attr.Value)
