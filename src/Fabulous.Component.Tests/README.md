# Component regression tests

Run from the repository root:

```powershell
dotnet run --project src/Fabulous.Component.Tests -c Release
```

This executable uses the real Fabulous widget diff, ViewNode, component builder and State bindings with
a minimal native-control substitute. It covers parent props and callbacks, state retention, comparator
skips, local renders after skips, comparison against the last rendered props, null and changing props
types, props without structural equality, initializers, modifiers, nested children, key changes,
disposal and attachment. Failed assertions terminate with a nonzero exit code.

It is separate from Fabulous.Tests because that existing suite currently targets net9.0 while the
library targets net10.0, and still uses older attribute/builder APIs that do not compile against this
checkout. RollForward=Major lets the executable run when only a newer .NET runtime is installed.

## NativeAOT smoke scenario

`SizeSmoke.fs` exercises ordinary components and optionally memo components with int, bool and string
props. It can compile against a saved baseline DLL (with memo disabled) or the current project:

```powershell
dotnet publish src/Fabulous.Component.Tests -c Release -r win-x64 `
    -p:PublishAot=true -p:ComponentSizeSmoke=true -p:IlcGenerateMapFile=true
```

Add `-p:ComponentMemoSmoke=true` to include the explicit props overload. To compare a saved assembly,
add `-p:FabulousAssembly=C:/path/to/Fabulous.dll`. Use separate scratch copies/output directories for
baseline, current and memo scenarios so incremental output is not mixed. Use the same compiler, runtime,
FSharp.Core version and source in all variants. The smoke output itself changes with the new behavior:
ordinary components print `a6` on the old implementation and `b6` on the new one.

A controlled win-x64 ILC comparison used .NET 11.0.100-rc.1.26425.128 and FSharp.Core
11.0.101-rc1.26425.128, with this Platform.fs and SizeSmoke.fs, against the saved original and updated
net10.0 Fabulous DLLs. Summing Length in the generated .map.xml gave:

| Scenario | All mapped bytes | Fabulous-named nodes | Comparer nodes / bytes |
| --- | ---: | ---: | ---: |
| Original, ordinary components | 3,275,905 | 143,640 | 1,616 / 83,665 |
| Updated, ordinary components | 3,274,537 | 141,505 | 1,598 / 82,889 |
| Updated, plus memo examples | 3,288,027 | 142,538 | 1,598 / 82,889 |

Comparer names were matched with `HashCompare|GenericEquality|GenericComparison|GenericComparer`.
The memo scenario includes additional application code and call sites; its total delta is not all
framework overhead. No additional comparer nodes appeared when adding the three props types.

ILC emitted the native objects and maps, but final linking was unavailable because the machine lacks
Visual C++ tools. `-p:IlcUseEnvironmentalTools=true` allowed compilation to finish before link.exe failed.
These numbers measure compiler output, not a linked executable or APK/IPA; actual mobile package deltas
require matched device-target publishes.
