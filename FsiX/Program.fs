// For more information see https://aka.ms/fsharp-console-apps

[<EntryPoint>]
let main args =
  FsiX.EntryPoint.main false args () |> _.GetAwaiter() |> _.GetResult()
  0
