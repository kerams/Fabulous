namespace Fabulous

open System
open Fabulous

/// Widget definition to create a control
type WidgetDefinition =
    { Key: WidgetKey
      Name: string
      TargetType: Type
      CreateView: Widget * ViewTreeContext * IViewNode voption -> struct (IViewNode * obj)
      AttachView: Widget * ViewTreeContext * IViewNode voption * obj -> IViewNode }

module WidgetDefinitionStore =
    let private _widgets = ResizeArray<WidgetDefinition>()

    let mutable private _nextKey = 0

    let get key = _widgets[key]

    // Both under the definition store lock: set writes into the list, which a concurrent getNextKey may be reallocating
    let set key value =
        AttributeDefinitionStore.SyncRoot.Enter()

        try
            _widgets[key] <- value
        finally
            AttributeDefinitionStore.SyncRoot.Exit()

    let getNextKey () : WidgetKey =
        AttributeDefinitionStore.SyncRoot.Enter()

        try
            _widgets.Add(Unchecked.defaultof<WidgetDefinition>)
            let key = _nextKey
            _nextKey <- _nextKey + 1
            key
        finally
            AttributeDefinitionStore.SyncRoot.Exit()
