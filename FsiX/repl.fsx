open System
open System.Threading.Tasks


/// Extensions for computation expressions to enable direct execution in the REPL.
/// These extensions allow users to write `let! someCode = someCompExpr` in the REPL,
/// where `someCompExpr` is a computation expression (e.g., `Async<'T>`, `Task<'T>`, or `Task`),
/// and have it executed synchronously without wrapping it in a computation expression block.
/// The REPL rewrites expressions like `let! x = someCompExpr` to `let x = someCompExpr.Run()`,
/// leveraging these extension methods to run the computation and extract its result.
///
/// To support additional computation expression types, define similar extension methods with a `Run`
/// member that executes the computation synchronously and returns the result.
/// For example, to support a custom computation expression `MyComp<'T>`, add:
/// ```fsharp
/// type MyComp<'T> with
///     member x.Run() = // Implementation to run synchronously
/// ```
type Async<'t> with
    member x.Run() = Async.RunSynchronously x

type Task<'t> with
    member x.Run() = x.GetAwaiter().GetResult()

type Task with
    member x.Run() = x.GetAwaiter().GetResult()

open PrettyPrompt
open PrettyPrompt.Consoles
open PrettyPrompt.Highlighting

let promptConfig =
    let mkP patterns =
        KeyPressPatterns(patterns |> Array.map (fun (modifier, key) -> KeyPressPattern(modifier, key)))

    PromptConfiguration(
        keyBindings =
            KeyBindings(
                commitCompletion = mkP [| (ConsoleModifiers.None, ConsoleKey.Tab) |],
                historyPrevious = mkP [| (ConsoleModifiers.Control, ConsoleKey.P) |],
                historyNext = mkP [| (ConsoleModifiers.Control, ConsoleKey.N) |]
            ),
        prompt =
            FormattedString(
                ">>> ",
                [| FormatSpan(0, 1, AnsiColor.Red)
                   FormatSpan(1, 1, AnsiColor.Yellow)
                   FormatSpan(2, 1, AnsiColor.Green) |]
            ),
        completionItemDescriptionPaneBackground = AnsiColor.Rgb(30uy, 30uy, 30uy),
        selectedCompletionItemBackground = AnsiColor.Rgb(30uy, 30uy, 30uy),
        selectedTextBackground = AnsiColor.Rgb(20uy, 61uy, 102uy)
    )
