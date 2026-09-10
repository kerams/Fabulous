namespace Fabulous

open Fabulous

module Reconciler =

    let update (canReuseView: Widget -> Widget -> bool) (prevOpt: inref<Widget voption>) (next: inref<Widget>) (node: IViewNode) : unit =

        let diff =
            WidgetDiff.create(prevOpt, next, canReuseView, fun key a b -> (AttributeDefinitionStore.getScalar key).CompareBoxed a b)

        node.ApplyDiff(&diff)
