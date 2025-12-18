open System


open FsiX.Daemon

open System.Net
open System.Net.Sockets

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
  let args = FsiX.Args.parseArgs args
  use listener = new TcpListener(addr, port);
  let rpc = Rpc.Tcp.mkJsonRpc listener |> _.GetAwaiter() |> _.GetResult()
  Rpc.createActorAndStartRpc args rpc
  rpc.Completion.GetAwaiter().GetResult()
  Console.ReadLine() |> ignore
  0
