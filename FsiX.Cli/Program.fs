// For more information see https://aka.ms/fsharp-console-apps
open FsiX.Cli

[<EntryPoint>]
let main args =
  CliEventLoop.runCliEventLoop false args () |> _.GetAwaiter() |> _.GetResult()
  0
