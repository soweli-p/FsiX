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
