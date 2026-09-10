namespace Fabulous

open System

/// This measure type is used to count the number of bindings in a component while building the computation expression
[<Measure>]
type binding

type ComponentBody =
    delegate of ViewTreeContext * ComponentContext -> struct (ViewTreeContext * ComponentContext * Widget)

[<Struct; NoEquality; NoComparison>]
type ComponentData = { Key: string; Body: ComponentBody }

type Component
    (componentDataKey: ScalarAttributeKey, treeContext: ViewTreeContext, context: ComponentContext, body: ComponentBody) =
    let mutable _treeContext = treeContext
    let mutable _context = context
    let mutable _body: ComponentBody | null = body
    let mutable _widget = Unchecked.defaultof<_>
    let mutable _view = null
    let mutable _contextSubscription: IDisposable | null = null

    let mutable _isReadyForRenderRequest = false
    let mutable _pendingRenderRequested = false

    member private this.MergeAttributes(rootWidget: inref<Widget>, componentWidgetOpt: inref<Widget voption>) =
        match componentWidgetOpt with
        | ValueNone ->
            struct (rootWidget.ScalarAttributes, rootWidget.WidgetAttributes, rootWidget.WidgetCollectionAttributes)

        | ValueSome componentWidget ->
            let componentScalars =
                match componentWidget.ScalarAttributes with
                | [||] -> [||]
                | attrs ->
                    let filteredAttrs =
                        attrs |> Array.filter(fun scalarAttr -> scalarAttr.Key <> componentDataKey)

                    filteredAttrs // skip the component data

            let scalars =
                match rootWidget.ScalarAttributes, componentScalars with
                | [||], [||] -> [||]
                | attrs, [||]
                | [||], attrs -> attrs
                | widgetAttrs, componentAttrs -> Array.append componentAttrs widgetAttrs

            let widgets =
                match rootWidget.WidgetAttributes, componentWidget.WidgetAttributes with
                | [||], [||] -> [||]
                | attrs, [||]
                | [||], attrs -> attrs
                | widgetAttrs, componentAttrs -> Array.append componentAttrs widgetAttrs

            let widgetColls =
                match rootWidget.WidgetCollectionAttributes, componentWidget.WidgetCollectionAttributes with
                | [||], [||] -> [||]
                | attrs, [||]
                | [||], attrs -> attrs
                | widgetAttrs, componentAttrs -> Array.append componentAttrs widgetAttrs

            struct (scalars, widgets, widgetColls)

    member this.CreateView(componentWidget: inref<Widget voption>) =
        _isReadyForRenderRequest <- false
        _contextSubscription <- _context.RenderNeeded.Subscribe(this.Render)

        let struct (treeContext, context, rootWidget) =
            _body.Invoke(_treeContext, _context)

        _widget <- rootWidget
        _treeContext <- treeContext
        _context <- context

        let struct (scalars, widgets, widgetColls) =
            this.MergeAttributes(&rootWidget, &componentWidget)

        let rootWidget: Widget =
            { Key = rootWidget.Key
#if DEBUG
              DebugName = rootWidget.DebugName
#endif
              ScalarAttributes = scalars
              WidgetAttributes = widgets
              WidgetCollectionAttributes = widgetColls }

        // Create the actual view
        let widgetDef = WidgetDefinitionStore.get rootWidget.Key

        let struct (node, view) =
            widgetDef.CreateView(rootWidget, treeContext, ValueNone)

        _view <- view
        _isReadyForRenderRequest <- true

        // ComponentContext.SetNeedsRender has been called before the view is created
        // We need to re-render the component now because the state has changed before we were ready
        if _pendingRenderRequested then
            _pendingRenderRequested <- false
            this.Render()

        struct (node, view)

    member this.AttachView(componentWidget: inref<Widget>, view: obj) =
        _isReadyForRenderRequest <- false
        _contextSubscription <- _context.RenderNeeded.Subscribe(this.Render)

        let struct (treeContext, context, rootWidget) =
            _body.Invoke(_treeContext, _context)

        _widget <- rootWidget
        _treeContext <- treeContext
        _context <- context
        let w = ValueSome componentWidget

        let struct (scalars, widgets, widgetColls) =
            this.MergeAttributes(&rootWidget, &w)

        let rootWidget: Widget =
            { Key = rootWidget.Key
#if DEBUG
              DebugName = rootWidget.DebugName
#endif
              ScalarAttributes = scalars
              WidgetAttributes = widgets
              WidgetCollectionAttributes = widgetColls }

        // Attach the widget to the existing view
        let widgetDef = WidgetDefinitionStore.get rootWidget.Key

        let node =
            widgetDef.AttachView(rootWidget, treeContext, ValueNone, view)

        _view <- view
        _isReadyForRenderRequest <- true

        // ComponentContext.SetNeedsRender has been called before the view is created
        // We need to re-render the component now because the state has changed before we were ready
        if _pendingRenderRequested then
            _pendingRenderRequested <- false
            this.Render()

        node

    member private this.RenderInternal() =
        if isNull _body then
            () // Component has been disposed
        else
            let prevRootWidget = _widget
            let prevContext = _context

            let struct (treeContext, context, currRootWidget) =
                _body.Invoke(_treeContext, _context)

            _widget <- currRootWidget
            _treeContext <- treeContext

            if prevContext <> context then
                _contextSubscription.Dispose()
                (prevContext :> IDisposable).Dispose()
                _contextSubscription <- context.RenderNeeded.Subscribe(this.Render)
                _context <- context

            let viewNode = treeContext.GetViewNode _view

            let prev = ValueSome prevRootWidget
            Reconciler.update treeContext.CanReuseView &prev &currRootWidget viewNode

    member this.Dispose() =
        if not(isNull _contextSubscription) then
            _contextSubscription.Dispose()

        if not(isNull _context) then
            (_context :> IDisposable).Dispose()

        _body <- null
        _widget <- Unchecked.defaultof<_>
        _view <- null
        _treeContext <- Unchecked.defaultof<_>
        _contextSubscription <- null
        _context <- null

    interface IDisposable with
        member this.Dispose() = this.Dispose()

    member this.Render() =
        if isNull _body then
            () // Component has been disposed
        else if not _isReadyForRenderRequest then
            _pendingRenderRequested <- true
        else
            treeContext.SyncAction(this.RenderInternal)
