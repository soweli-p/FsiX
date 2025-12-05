open System
open StreamJsonRpc
open Nerdbank.Streams


open FsiX.Daemon
open FsiX.Daemon.Json

let mkStdioJsonRpc () =
  let stream = FullDuplexStream.Splice(Console.OpenStandardInput(), Console.OpenStandardOutput())
  let handler = new HeaderDelimitedMessageHandler(stream, mkJsonFormatter ())
  new JsonRpc(handler)

[<EntryPoint>]
let main args =
  let rpc = mkStdioJsonRpc ()
  Rpc.startAndInitRpc args rpc
  rpc.Completion.GetAwaiter().GetResult()
  Console.ReadLine() |> ignore
  0
