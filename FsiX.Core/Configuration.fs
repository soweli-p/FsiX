module FsiX.Configuration

open System
open System.IO
open System.Reflection
open FSharpPlus

let prettyPromptDll =
    typeof<PrettyPrompt.PromptConfiguration> |> _.Assembly |> _.Location

let loadDllString = $"#r \"{prettyPromptDll}\"\n" |> _.Replace(@"\", @"\\")

let getDefaultConfig () =
    task {
        let! defaultConfig =
            let asm = Assembly.GetExecutingAssembly()
            use stream = asm.GetManifestResourceStream $"FsiX.repl.fsx"
            use reader = new StreamReader(stream)
            reader.ReadToEndAsync()

        return loadDllString + defaultConfig
    }

let patchDllIfNeeded configCode =
    let lines = String.split [ "\n" ] configCode

    match lines |> Seq.tryFind (_.Contains("PrettyPrompt.dll")) with
    | None -> Some <| loadDllString + configCode
    | Some actualLine when actualLine <> loadDllString -> configCode |> String.replace actualLine loadDllString |> Some
    | Some _ -> None

let loadGlobalConfig () =
    task {
        let configPath =
            Environment.GetFolderPath Environment.SpecialFolder.ApplicationData
            |> fun s -> Path.Combine [| s; "fsix"; "repl.fsx" |]

        let patchDllIfNeeded configCode =
            match patchDllIfNeeded configCode with
            | None -> Task.result configCode
            | Some patchedCode ->
                File.WriteAllTextAsync(configPath, patchedCode) |> Task.ignore
                |>> konst patchedCode

        if File.Exists configPath then
            return! File.ReadAllTextAsync configPath >>= patchDllIfNeeded
        else
            configPath 
              |> Path.GetDirectoryName
              |> Directory.CreateDirectory
              |> ignore
            let! defaultConfig = getDefaultConfig ()
            do! File.WriteAllTextAsync(configPath, defaultConfig)
            return defaultConfig
    }
