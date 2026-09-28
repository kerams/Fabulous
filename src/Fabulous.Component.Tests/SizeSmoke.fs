module Smoke
open Fabulous
open type Fabulous.Context
open ComponentTests.Platform

let mutable setCount = ignore<int>
let regular label =
    ComponentBuilder<unit, Marker>("smoke") {
        let! count = State 0
        setCount <- count.Set
        control (label + string count.Current) (fun () -> count.Set(count.Current + 1))
    }

#if MEMO
// Each call is deliberately inlined: only the comparator adapter specializes over props.
let inline memo key equal props label =
    ComponentBuilder<unit, Marker>.Create(key, props, equal) {
        let! count = State 0
        control (label + string count.Current) (fun () -> count.Set(count.Current + 1))
    }
#endif

[<EntryPoint>]
let main _ =
    let first = regular "a"
    let view = mount first
    setCount 5
    update view first (regular "b")
    view.Click()
    printfn "%s" view.Text
#if MEMO
    let first = memo "int" (fun (a: int) b -> a = b) 1 "int"
    let view = mount first
    update view first (memo "int" (fun (a: int) b -> a = b) 1 "int")
    update view first (memo "int" (fun (a: int) b -> a = b) 2 "int")
    view.Click()
    printfn "%s" view.Text
    let first = memo "bool" (fun (a: bool) b -> a = b) false "bool"
    let view = mount first
    update view first (memo "bool" (fun (a: bool) b -> a = b) true "bool")
    printfn "%s" view.Text
    let first = memo "string" (fun (a: string) b -> System.String.Equals(a, b, System.StringComparison.Ordinal)) "a" "string"
    let view = mount first
    update view first (memo "string" (fun (a: string) b -> System.String.Equals(a, b, System.StringComparison.Ordinal)) "b" "string")
    printfn "%s" view.Text
#endif
    0
