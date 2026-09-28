namespace Fabulous

open System

module Component' =
    let Data =
        Attributes.defineSimpleScalar<ComponentData> "Component_Data" ScalarAttributeComparers.noCompare (fun _ next node ->
            if next.HasValue then
                match node.TreeContext.GetComponent(node.Target) with
                | :? Component as comp -> comp.Update(next.Value)
                | _ -> ())

    let WidgetKey =
        let key = WidgetDefinitionStore.getNextKey()

        let definition =
            { Key = key
              Name = "Component"
              TargetType = typeof<Component>
              CreateView =
                fun (widget, treeContext, _) ->
                    let data =
                        match widget.ScalarAttributes |> Array.tryFind(fun scalarAttr -> scalarAttr.Key = Data.Key) with
                        | Some attr -> Unchecked.nonNull attr.Value :?> ComponentData
                        | None -> failwith "Component widget must have a body"

                    let context = new ComponentContext()
                    let comp = new Component(Data.Key, treeContext, context, data)
                    let w = ValueSome widget
                    let struct (node, view) = comp.CreateView(&w)

                    treeContext.SetComponent comp view

                    struct (node, view)
              AttachView =
                fun (widget, treeContext, _, view) ->
                    let data =
                        match widget.ScalarAttributes |> Array.tryFind(fun scalarAttr -> scalarAttr.Key = Data.Key) with
                        | Some attr -> Unchecked.nonNull attr.Value :?> ComponentData
                        | None -> failwith "Component widget must have a body"

                    let context = new ComponentContext()
                    let comp = new Component(Data.Key, treeContext, context, data)
                    let node = comp.AttachView(&widget, view)

                    treeContext.SetComponent comp view

                    node }

        WidgetDefinitionStore.set key definition
        key

    let canReuseComponent (prev: inref<Widget>) (curr: inref<Widget>) =
        let prevData =
            match prev.ScalarAttributes |> Array.tryFind(fun scalarAttr -> scalarAttr.Key = Data.Key) with
            | None -> failwith "Component widget must have a body"
            | Some value -> Unchecked.nonNull value.Value :?> ComponentData

        let currData =
            match curr.ScalarAttributes |> Array.tryFind(fun scalarAttr -> scalarAttr.Key = Data.Key) with
            | None -> failwith "Component widget must have a body"
            | Some value -> Unchecked.nonNull value.Value :?> ComponentData

        // NOTE: Somehow using = here crashes the app and prevents debugging...
        Object.Equals(prevData.Key, currData.Key)
