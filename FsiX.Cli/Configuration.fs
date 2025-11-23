module FsiX.Configuration

open System
open System.IO
open FSharpPlus
open FsiX.Utils.Configuration

let prettyPromptDll =
    typeof<PrettyPrompt.PromptConfiguration> |> _.Assembly |> _.Location

let loadDllString = $"#r \"{prettyPromptDll}\"" |> _.Replace(@"\", @"\\")


let getDefaultConfig () =
    task {
        let! baseCfg = getEmbeddedFileAsString "FsiX.base.fsx"
        and! promptCfg = getEmbeddedFileAsString "FsiX.Cli.prompt.fsx" 

        return String.concat Environment.NewLine [
          loadDllString
          baseCfg
          promptCfg
        ]
    }

let patchDllIfNeeded configCode =
    let lines = String.split [ Environment.NewLine ] configCode

    match lines |> Seq.tryFind (_.Contains("PrettyPrompt.dll")) with
    | None -> Some <| loadDllString + configCode
    | Some actualLine when actualLine <> loadDllString -> configCode |> String.replace actualLine loadDllString |> Some
    | Some _ -> None

let loadGlobalConfig () =
    task {
        let dirPath = getConfigDir ()
        let configPath = Path.Combine(dirPath, "repl.fsx")

        let patchDllIfNeeded configCode =
            match patchDllIfNeeded configCode with
            | None -> Task.result configCode
            | Some patchedCode ->
                File.WriteAllTextAsync(configPath, patchedCode) |> Task.ignore
                |>> konst patchedCode

        if File.Exists configPath then
            return! File.ReadAllTextAsync configPath >>= patchDllIfNeeded
        else
            let! defaultConfig = getDefaultConfig ()
            do! File.WriteAllTextAsync(configPath, defaultConfig)
            return defaultConfig
    }
