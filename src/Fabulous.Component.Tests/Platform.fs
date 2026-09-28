module ComponentTests.Platform

open System
open Fabulous

[<AllowNullLiteral>]
type Control() =
    member val Text = "" with get, set
    member val Color = "" with get, set
    member val Click = (fun () -> ()) with get, set
    member val Child: Control = null with get, set
    member val Node = Unchecked.defaultof<IViewNode> with get, set
    member val Component: obj = null with get, set

type Marker = interface end

let text =
    Attributes.defineSimpleScalar<string> "Text" ScalarAttributeComparers.equalityCompare (fun _ next node ->
        (node.Target :?> Control).Text <- if next.HasValue then next.Value else "")

let color =
    Attributes.defineSimpleScalar<string> "Color" ScalarAttributeComparers.equalityCompare (fun _ next node ->
        (node.Target :?> Control).Color <- if next.HasValue then next.Value else "")

let click =
    Attributes.defineSimpleScalar<unit -> unit> "Click" ScalarAttributeComparers.noCompare (fun _ next node ->
        (node.Target :?> Control).Click <- if next.HasValue then next.Value else (fun () -> ()))

let child =
    Attributes.definePropertyWidget<Control> "Child" (fun target -> (target :?> Control).Child) (fun target value ->
        (target :?> Control).Child <- value)

let treeContext =
    { CanReuseView = fun previous next -> ViewHelpers.canReuseView &previous &next
      GetViewNode = fun target -> (target :?> Control).Node
#if DEBUG
      Logger = { Log = ignore; MinLogLevel = LogLevel.Fatal }
#endif
      Dispatch = ignore
      SyncAction = fun action -> action()
      GetComponent = fun target -> (target :?> Control).Component
      SetComponent = fun value target -> (target :?> Control).Component <- value }

// Use the real ViewNode and reconciler; only native controls are replaced by this tiny test platform.
let widgetKey =
    let key = WidgetDefinitionStore.getNextKey()
    let attach (widget: Widget) (context: ViewTreeContext) parent (view: Control) =
        let node = new ViewNode(parent, &context, WeakReference(view)) :> IViewNode
        view.Node <- node
        let previous = ValueNone
        Reconciler.update context.CanReuseView &previous &widget node
        node
    WidgetDefinitionStore.set key
        { Key = key
          Name = "Control"
          TargetType = typeof<Control>
          CreateView = fun (widget, context, parent) ->
              let view = Control()
              let node = attach widget context (ValueOption.toOption parent) view
              struct (node, box view)
          AttachView = fun (widget, context, parent, target) ->
              attach widget context (ValueOption.toOption parent) (target :?> Control) }
    key

let control value callback =
    WidgetBuilder<unit, Marker>(widgetKey)
        .AddScalar(text.WithValue(value))
        .AddScalar(click.WithValue(callback))

let host (content: WidgetBuilder<unit, Marker>) =
    let attr = child.WithValue(content.Compile())
    WidgetBuilder<unit, Marker>(widgetKey).AddWidget(&attr)

let mount (builder: WidgetBuilder<unit, Marker>) =
    let widget = builder.Compile()
    let struct (_, target) = (WidgetDefinitionStore.get widget.Key).CreateView(widget, treeContext, ValueNone)
    target :?> Control

let update (view: Control) (previous: WidgetBuilder<unit, Marker>) (next: WidgetBuilder<unit, Marker>) =
    let previous = ValueSome(previous.Compile())
    let next = next.Compile()
    Reconciler.update treeContext.CanReuseView &previous &next view.Node

let check message condition =
    if not condition then failwith message
