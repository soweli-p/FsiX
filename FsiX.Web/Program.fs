// For more information see https://aka.ms/fsharp-console-apps

[<EntryPoint>]
let main args =
  FsiX.EntryPoint.main true args () |> _.GetAwaiter() |> _.GetResult()
  0
