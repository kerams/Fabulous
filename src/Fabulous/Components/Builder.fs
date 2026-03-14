namespace Fabulous

/// Delegate used by the ComponentBuilder to compose a component body
/// It will be aggressively inlined by the compiler leaving no overhead, only a pure function that returns a WidgetBuilder
type ComponentBodyBuilder<'msg, 'marker when 'msg: equality> =
    delegate of
        treeContext: ViewTreeContext * context: ComponentContext * bindings: int<binding> ->
            struct (ViewTreeContext * ComponentContext * int<binding> * WidgetBuilder<'msg, 'marker>)

type ComponentBuilder<'parentMsg, 'marker when 'parentMsg: equality> =
    val public Key: string

    new(key: string) = { Key = key }

    member inline this.Yield(widgetBuilder: WidgetBuilder<'msg, 'marker>) =
        ComponentBodyBuilder<'msg, 'marker>(fun treeContext context bindings -> struct (treeContext, context, bindings, widgetBuilder))

    member inline this.Combine([<InlineIfLambda>] a: ComponentBodyBuilder<'msg, 'marker>, [<InlineIfLambda>] b: ComponentBodyBuilder<'msg, 'marker>) =
        ComponentBodyBuilder<'msg, 'marker>(fun treeContext context bindings ->
            let struct (treeA, ctxA, bindingsA, _) =
                a.Invoke(treeContext, context, bindings) // discard the previous widget in the chain but we still need to count the bindings

            let struct (treeB, ctxB, bindingsB, widgetB) =
                b.Invoke(treeA, ctxA, bindingsA)

            // Calculate the total number of bindings between A and B
            let combinedBindings = (bindingsA + bindingsB) - bindings

            struct (treeB, ctxB, combinedBindings, widgetB))

    member inline this.Delay([<InlineIfLambda>] fn: unit -> ComponentBodyBuilder<'msg, 'marker>) =
        ComponentBodyBuilder<'msg, 'marker>(fun treeContext context bindings ->
            let sub = fn()
            sub.Invoke(treeContext, context, bindings))

    member inline this.Run([<InlineIfLambda>] body: ComponentBodyBuilder<'msg, 'marker>) =
        let compiledBody =
            ComponentBody(fun treeContext context ->
                let struct (treeA, ctxA, _, result) =
                    body.Invoke(treeContext, context, 0<binding>)

                struct (treeA, ctxA, result.Compile()))

        let data = { Key = this.Key; Body = compiledBody }

        WidgetBuilder<'parentMsg, 'marker>(Component'.WidgetKey, Component'.Data.WithValue(data))
