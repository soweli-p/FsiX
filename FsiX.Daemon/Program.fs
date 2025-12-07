open System
open StreamJsonRpc
open Nerdbank.Streams


open FsiX.Daemon
open FsiX.Daemon.Json

open System.Net
open System.Net.Sockets

let mkStdioJsonRpc (listener: TcpListener) =
  listener.Start()
  Console.WriteLine $"Listening on: {listener.LocalEndpoint.ToString()}"
  let client = listener.AcceptTcpClient()
  let stream = client.GetStream()
  let handler = new HeaderDelimitedMessageHandler(stream, mkJsonFormatter ())
  new JsonRpc(handler)

open FSharpPlus
let tryParseIp args = 
  match args |> Array.tryHead |>> String.split [":"] |>> Seq.toList with 
  | Some [ipPart; portPart] -> Some (IPAddress.Parse ipPart, Int32.Parse portPart, Array.skip 1 args)
  | Some [ipOnlyPart] when IPAddress.TryParse ipOnlyPart |> fst -> Some (IPAddress.Parse ipOnlyPart, 0, Array.skip 1 args)
  | _ -> None
[<EntryPoint>]
let main args =
  let addr, port, args = 
    tryParseIp args |> Option.defaultValue (IPAddress.Loopback, 0, args)
  use listener = new TcpListener(addr, port);
  let rpc = mkStdioJsonRpc listener
  Rpc.startAndInitRpc args rpc
  rpc.Completion.GetAwaiter().GetResult()
  Console.ReadLine() |> ignore
  0
