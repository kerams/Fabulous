namespace Fabulous

open System.ComponentModel
open Fabulous.WidgetCollectionAttributeDefinitions
open Fabulous.StackAllocatedCollections
open Fabulous.StackAllocatedCollections.StackList
open Microsoft.FSharp.Core

type AttributesBundle = (struct (StackList<ScalarAttribute> * WidgetAttribute[] * WidgetCollectionAttribute[]))

[<Struct; NoComparison; NoEquality>]
type WidgetBuilder<'msg, 'marker when 'msg: equality> =
    struct
        val Key: WidgetKey
        val Attributes: AttributesBundle

        new(key: WidgetKey) =
            { Key = key
              Attributes = AttributesBundle(StackList.empty(), [||], [||]) }

        new(key: WidgetKey, attributes: inref<AttributesBundle>) = { Key = key; Attributes = attributes }

        new(key: WidgetKey, scalar: inref<ScalarAttribute>) =
            { Key = key
              Attributes = AttributesBundle(StackList.one scalar, [||], [||]) }

        new(key: WidgetKey, scalarA: inref<ScalarAttribute>, scalarB: inref<ScalarAttribute>) =
            { Key = key
              Attributes = AttributesBundle(StackList.two(scalarA, scalarB), [||], [||]) }

        new(key: WidgetKey, scalar1: inref<ScalarAttribute>, scalar2: inref<ScalarAttribute>, scalar3: inref<ScalarAttribute>) =
            { Key = key
              Attributes = AttributesBundle(StackList.three(scalar1, scalar2, scalar3), [||], [||]) }

        new(key: WidgetKey, widget: inref<WidgetAttribute>) =
            { Key = key
              Attributes = AttributesBundle(StackList.empty(), [| widget |], [||]) }

        [<EditorBrowsable(EditorBrowsableState.Never)>]
        member x.Compile() : Widget =
            let struct (scalarAttributes, widgetAttributes, widgetCollectionAttributes) =
                x.Attributes

            { Key = x.Key
#if DEBUG
              DebugName = $"{typeof<'marker>.Name}<{typeof<'msg>.Name}>"
#endif
              ScalarAttributes =
                match StackList.length &scalarAttributes with
                | 0us -> [||]
                | _ -> Array.sortInPlace _.Key (StackList.toArray &scalarAttributes)

              WidgetAttributes =
                if widgetAttributes.Length > 1 then
                    Array.sortInPlace _.Key widgetAttributes
                else
                    widgetAttributes

              WidgetCollectionAttributes =
                if widgetCollectionAttributes.Length > 1 then
                    Array.sortInPlace _.Key widgetCollectionAttributes
                else
                    widgetCollectionAttributes }

        [<EditorBrowsable(EditorBrowsableState.Never)>]
        member inline x.AddScalar(attr: ScalarAttribute) =
            let struct (scalarAttributes, widgetAttributes, widgetCollectionAttributes) =
                x.Attributes

            let atts = struct (StackList.add(&scalarAttributes, &attr), widgetAttributes, widgetCollectionAttributes)

            WidgetBuilder<'msg, 'marker>(
                x.Key,
                &atts
            )

        [<EditorBrowsable(EditorBrowsableState.Never)>]
        member inline x.AddOrReplaceScalar
            (
                attrKey: ScalarAttributeKey,
                [<InlineIfLambda>] replaceWith: ScalarAttribute -> ScalarAttribute,
                [<InlineIfLambda>] defaultWith: unit -> ScalarAttribute
            ) =
            let struct (scalarAttributes, widgetAttributes, widgetCollectionAttributes) =
                x.Attributes

            match StackList.tryFind(&scalarAttributes, (fun attr -> attr.Key = attrKey)) with
            | ValueNone ->
                let attr = defaultWith()
                let atts = struct (StackList.add(&scalarAttributes, &attr), widgetAttributes, widgetCollectionAttributes)

                WidgetBuilder<'msg, 'marker>(
                    x.Key,
                    &atts
                )

            | ValueSome attr ->
                let newAttr = replaceWith attr

                let newAttrs =
                    StackList.replace(&scalarAttributes, (fun attr -> attr.Key = attrKey), newAttr)

                let atts = struct (newAttrs, widgetAttributes, widgetCollectionAttributes)
                WidgetBuilder<'msg, 'marker>(x.Key, &atts)

        [<EditorBrowsable(EditorBrowsableState.Never)>]
        member x.AddWidget(attr: inref<WidgetAttribute>) =
            let struct (scalarAttributes, widgetAttributes, widgetCollectionAttributes) =
                x.Attributes

            let attribs = widgetAttributes

            let res =
                match attribs with
                | [||] -> [| attr |]
                | attribs -> Array.appendOne attr attribs

            let atts = struct (scalarAttributes, res, widgetCollectionAttributes)
            WidgetBuilder<'msg, 'marker>(x.Key, &atts)

        [<EditorBrowsable(EditorBrowsableState.Never)>]
        member x.AddWidgetCollection(attr: inref<WidgetCollectionAttribute>) =
            let struct (scalarAttributes, widgetAttributes, widgetCollectionAttributes) =
                x.Attributes

            let attribs = widgetCollectionAttributes

            let res =
                match attribs with
                | [||] -> [| attr |]
                | attribs -> Array.appendOne attr attribs

            let atts = struct (scalarAttributes, widgetAttributes, res)
            WidgetBuilder<'msg, 'marker>(x.Key, &atts)

    end



[<Struct>]
type Content<'msg> = { Widgets: MutStackArray1.T<Widget> }

[<Struct; NoComparison; NoEquality>]
type CollectionBuilder<'msg, 'marker, 'itemMarker when 'msg: equality> =
    struct
        val WidgetKey: WidgetKey
        val Attr: WidgetCollectionAttributeDefinition
        val Attributes: AttributesBundle

        new(widgetKey: WidgetKey, scalars: StackList<ScalarAttribute>, attr: WidgetCollectionAttributeDefinition) =
            { WidgetKey = widgetKey
              Attributes = AttributesBundle(scalars, [||], [||])
              Attr = attr }

        new(widgetKey: WidgetKey, attr: WidgetCollectionAttributeDefinition) =
            { WidgetKey = widgetKey
              Attributes = AttributesBundle(StackList.empty(), [||], [||])
              Attr = attr }

        new(widgetKey: WidgetKey, attr: WidgetCollectionAttributeDefinition, attributes: AttributesBundle) =
            { WidgetKey = widgetKey
              Attributes = attributes
              Attr = attr }

        new(widgetKey: WidgetKey, attr: WidgetCollectionAttributeDefinition, scalar: ScalarAttribute) =
            { WidgetKey = widgetKey
              Attributes = AttributesBundle(StackList.one scalar, [||], [||])
              Attr = attr }

        new(widgetKey: WidgetKey, attr: WidgetCollectionAttributeDefinition, scalarA: ScalarAttribute, scalarB: ScalarAttribute) =
            { WidgetKey = widgetKey
              Attributes = AttributesBundle(StackList.two(scalarA, scalarB), [||], [||])
              Attr = attr }

        member inline x.Run(c: Content<'msg>) =
            let struct (scalars, widgets, widgetCollections) = x.Attributes

            let attrValue =
                match MutStackArray1.toArraySlice &c.Widgets with
                | ValueNone -> ArraySlice.emptyWithNull()
                | ValueSome slice -> slice

            let widgetCollAttr = x.Attr.WithValue(attrValue)

            let widgetCollections =
                match widgetCollections with
                | [||] -> [| widgetCollAttr |]
                | widgetCollectionAttributes -> Array.appendOne widgetCollAttr widgetCollectionAttributes

            let atts = AttributesBundle(scalars, widgets, widgetCollections)
            WidgetBuilder<'msg, 'marker>(x.WidgetKey, &atts)

        member inline _.Combine(a: Content<'msg>, b: Content<'msg>) : Content<'msg> =
            let res = MutStackArray1.combineMut(&a.Widgets, &b.Widgets)

            { Widgets = res }

        member inline _.Zero() : Content<'msg> = { Widgets = MutStackArray1.Empty }

        member inline _.Delay([<InlineIfLambda>] f) : Content<'msg> = f()

        member inline x.For<'t>(sequence: 't seq, f: 't -> Content<'msg>) : Content<'msg> =
            let mutable res: Content<'msg> = x.Zero()

            // this is essentially Fold, not sure what is more optimal
            // handwritten version of via Seq.Fold
            for t in sequence do
                res <- x.Combine(res, f t)

            res

        member inline _.YieldFrom(sequence: WidgetBuilder<'msg, 'itemMarker> seq) : Content<'msg> =
            // TODO optimize this one with addMut
            { Widgets =
                sequence
                |> Seq.map(fun wb -> wb.Compile())
                |> Seq.toArray
                |> MutStackArray1.fromArray }

        [<EditorBrowsable(EditorBrowsableState.Never)>]
        member inline x.AddScalar(attr: inref<ScalarAttribute>) =
            let struct (scalarAttributes, widgetAttributes, widgetCollectionAttributes) =
                x.Attributes

            CollectionBuilder<'msg, 'marker, 'itemMarker>(
                x.WidgetKey,
                x.Attr,
                struct (StackList.add(&scalarAttributes, &attr), widgetAttributes, widgetCollectionAttributes)
            )

    end

module CollectionBuilder =
    let inline yieldImpl (builder: inref<WidgetBuilder<'msg, 'itemMarker>>) : Content<'msg> =
        { Widgets = MutStackArray1.One(builder.Compile()) }

[<Struct>]
type AttributeCollectionBuilder<'msg, 'marker, 'itemMarker when 'msg: equality> =
    struct
        val Widget: WidgetBuilder<'msg, 'marker>
        val Attr: WidgetCollectionAttributeDefinition

        new(widget: inref<WidgetBuilder<'msg, 'marker>>, attr: inref<WidgetCollectionAttributeDefinition>) = { Widget = widget; Attr = attr }

        member inline x.Run(c: Content<'msg>) =
            let attrValue =
                match MutStackArray1.toArraySlice &c.Widgets with
                | ValueNone -> ArraySlice.emptyWithNull()
                | ValueSome slice -> slice

            let w = x.Attr.WithValue(attrValue)
            x.Widget.AddWidgetCollection &w

        member inline _.Combine(a: Content<'msg>, b: Content<'msg>) : Content<'msg> =
            { Widgets = MutStackArray1.combineMut(&a.Widgets, &b.Widgets) }

        member inline _.Zero() : Content<'msg> = { Widgets = MutStackArray1.Empty }

        member inline _.Delay([<InlineIfLambda>] f) : Content<'msg> = f()

        member inline x.For<'t>(sequence: 't seq, [<InlineIfLambda>] f: 't -> Content<'msg>) : Content<'msg> =
            let mutable res: Content<'msg> = x.Zero()

            // this is essentially Fold, not sure what is more optimal
            // handwritten version of via Seq.Fold
            for t in sequence do
                res <- x.Combine(res, f t)

            res
    end
