module FsiX.Cli.Logging

open FsiX.Utils

module private Logging =
  let logInfo = printfn "\u001b[90m %s \u001b[0m"
  let logDebug = printfn "\u001b[90m %s \u001b[0m"
  let logWarning = printfn "\u001b[33m %s \u001b[0m"
  let logError = printfn "\u001b[31m %s \u001b[0m"

type CliLogger() =
    interface ILogger with
        member this.LogDebug s = Logging.logDebug s
        member this.LogInfo s = Logging.logInfo s
        member this.LogError s = Logging.logError s
        member this.LogWarning s = Logging.logWarning s

let cliLogger: ILogger = CliLogger()
