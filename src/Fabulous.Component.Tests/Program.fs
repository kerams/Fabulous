module ComponentTests.Program

open System
open Fabulous
open type Fabulous.Context
open ComponentTests.Platform

[<Struct; NoEquality; NoComparison>]
type Props = { Number: int; Callback: unit -> unit }

let mutable renders = 0
let mutable setter = ignore<int>
let mutable received = ""

let counter (builder: ComponentBuilder<unit, Marker>) label =
    builder {
        let! count = State 0
        renders <- renders + 1
        setter <- count.Set
        control (label + ":" + string count.Current) (fun () -> received <- label)
    }

let regular label = counter (ComponentBuilder<unit, Marker>("counter")) label
let memo props label = counter (ComponentBuilder<unit, Marker>.Create("counter", props, (=))) label

let parentUpdates () =
    let first = regular "old"
    let view = mount first
    setter 7
    let next = regular "new"
    update view first next
    check "Parent update preserves state and refreshes captured props" (view.Text = "new:7")
    view.Click()
    check "Callbacks use new props" (received = "new")
    setter 8
    check "Local update uses the latest parent body" (view.Text = "new:8")
    let instance = view.Component :?> Component
    view.Node.Dispose()
    instance.Render()
    check "Disposed components stop rendering" (view.Text = "new:8")

let memoUpdates () =
    let first = memo 1 "one"
    let view = mount first
    let count = renders
    let skipped = memo 1 "latest"
    update view first skipped
    check "Equal props skip body evaluation" (renders = count && view.Text = "one:0")
    // Reconciliation would replace this handler even if the text didn't change.
    view.Click()
    check "Equal props skip child reconciliation" (received = "one")
    setter 3
    check "Local state bypasses memo and sees latest props" (view.Text = "latest:3")
    let changed = memo 2 "two"
    update view skipped changed
    check "Unequal props render and preserve state" (view.Text = "two:3")
    let plain = regular "plain"
    update view changed plain
    check "Removing memo renders" (view.Text = "plain:3")
    let again = memo 2 "again"
    update view plain again
    check "Adding memo renders" (view.Text = "again:3")

let comparisonBaseline () =
    let build n =
        counter (ComponentBuilder<unit, Marker>.Create("counter", n, (fun a b -> abs(a - b) < 2))) (string n)
    let first = build 0
    let view = mount first
    let skipped = build 1
    update view first skipped
    check "Near props skipped" (view.Text = "0:0")
    let next = build 2
    update view skipped next
    check "Compare with last rendered props, not last skipped props" (view.Text = "2:0")

let propsKinds () =
    let first = memo (None: string option) "none"
    let view = mount first
    let next = memo (None: string option) "skipped"
    let count = renders
    update view first next
    check "Null-represented props can be memoized" (renders = count)
    let changed = memo (Some "a") "some"
    update view next changed
    check "Option props change" (view.Text = "some:0")
    let otherType = memo 1 "int"
    update view changed otherType
    check "Changing props type does not invoke an invalid comparer" (view.Text = "int:0")
    let withCallback =
        counter (ComponentBuilder<unit, Marker>.Create("counter", Action(ignore), (fun (a: Action) b -> obj.ReferenceEquals(a, b)))) "callback"
    update view otherType withCallback
    check "Props have no equality constraint" (view.Text = "callback:0")

let explicitComparer () =
    let build props =
        ComponentBuilder<unit, Marker>.Create("props", props, (fun a b -> a.Number = b.Number && obj.ReferenceEquals(a.Callback, b.Callback))) {
            let! count = State props.Number
            setter <- count.Set
            control (string count.Current) props.Callback
        }
    let first = build { Number = 4; Callback = fun () -> received <- "first" }
    let view = mount first
    setter 9
    let next = build { Number = 6; Callback = fun () -> received <- "second" }
    update view first next
    check "Changed State default does not reset an existing slot" (view.Text = "9")
    view.Click()
    check "Struct props without structural equality support explicit comparers and callbacks" (received = "second")

let modifiers () =
    let first = (memo 1 "one").AddScalar(color.WithValue("red"))
    let view = mount first
    let count = renders
    let next = (memo 1 "one").AddScalar(color.WithValue("blue"))
    update view first next
    check "Outer modifiers update despite memo" (view.Color = "blue" && renders = count)
    setter 1
    check "Outer modifiers survive local rendering" (view.Color = "blue")
    let removed = memo 1 "one"
    update view next removed
    check "Outer modifiers can be removed" (view.Color = "")

let nestedAndKeys () =
    let outer key label =
        ComponentBuilder<unit, Marker>("outer") {
            host (counter (ComponentBuilder<unit, Marker>(key)) label)
        }
    let first = outer "child" "old"
    let view = mount first
    let original = view.Child
    setter 4
    let next = outer "child" "new"
    update view first next
    check "Nested components retain native view and state" (obj.ReferenceEquals(original, view.Child) && view.Child.Text = "new:4")
    let reset = outer "different-child" "reset"
    update view next reset
    check "Changed key remounts and resets state" (not(obj.ReferenceEquals(original, view.Child)) && view.Child.Text = "reset:0")

let attachment () =
    let first = regular "attached"
    let widget = first.Compile()
    let view = Control()
    (WidgetDefinitionStore.get widget.Key).AttachView(widget, treeContext, ValueNone, view) |> ignore
    setter 6
    let next = regular "updated"
    update view first next
    check "Attached components update with retained state" (view.Text = "updated:6")

[<EntryPoint>]
let main _ =
    for name, test in
        [ "parent updates", parentUpdates
          "memo updates", memoUpdates
          "comparison baseline", comparisonBaseline
          "props kinds", propsKinds
          "explicit comparer", explicitComparer
          "modifiers", modifiers
          "nested components and keys", nestedAndKeys
          "attachment", attachment ] do
        test()
        printfn "PASS %s" name
    0
