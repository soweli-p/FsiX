module FsiX.AppState

open System
open System.IO

open System.Threading
open System.Threading.Tasks
open FSharp.Compiler.Interactive.Shell
open FSharpPlus
open FsiX.Features
open FsiX.ProjectLoading
open FsiX.Utils

open PrettyPrompt

type FilePath = string


type BufferedStdoutWriter() =
    inherit TextWriter()

    let realStdout = Console.Out
    let mutable isEnabled = false

    override _.Encoding = realStdout.Encoding

    override _.Write(value: char) =
        if isEnabled then realStdout.Write value

    override _.Write(value: string) =
        if isEnabled then realStdout.Write value

    override _.Write(bufferArr: char[], index: int, count: int) =
        if isEnabled then realStdout.Write(bufferArr, index, count)

    member _.Enable() =
      isEnabled <- true

    override _.Flush() =
        realStdout.Flush()



open FsiX.Features.Reloading
type AppState private (sln: Solution, session: FsiEvaluationSession, globalConfig, localConfigs, outStream) =
    let mutable reloadingState = mkReloadingState sln
    member _.Solution = sln
    member _.InteractiveChecker = session.InteractiveChecker

    member this.EvalCode(code, token) =
        try
            session.EvalInteraction(code, token)
            if code.Contains "open" then
              reloadingState <- getOpenModules code reloadingState
            reloadingState <- handleNewAsmFromRepl (Array.last session.DynamicAssemblies) reloadingState
            //session.AddBoundValue("assemblies", session.DynamicAssemblies)
        with _ ->
            ()
    member _.OutStream = outStream

    member this.GetPromptConfiguration() =
        printfn "Loading configuration..."
        this.EvalCode(globalConfig, CancellationToken.None)

        for cfg in localConfigs do
          this.EvalCode(cfg, CancellationToken.None)

        let PromptConfigurationValue =
            session.GetBoundValues()
            |> List.tryFind (fun x -> x.Value.ReflectionType.Name = nameof PromptConfiguration)

        match PromptConfigurationValue with
        | None -> failwith "No PromptConfiguration was found!"
        | Some v -> (v.Value.ReflectionValue :?> PromptConfiguration)

    member _.GetCompletions(text, caret, word) =
        AutoCompletion.getCompletions session text caret word

    static member mkAppState useAsp sln =
        task {
            printfn "Welcome to FsiX!"
            printfn "Loading these projects: "
            for project in sln.Projects do
              printfn "%s" project.ProjectFileName
            let globalConfigTask = Configuration.loadGlobalConfig ()
            let localConfigTasks = sln.StartupFiles |> Seq.map File.ReadAllTextAsync |> Task.WhenAll
            let fsiConfig = FsiEvaluationSession.GetDefaultConfiguration()
            let args = solutionToFsiArgs useAsp sln


            let out = new BufferedStdoutWriter()
            let fsiSession =
                FsiEvaluationSession.Create(
                    fsiConfig,
                    args,
                    new StreamReader(Stream.Null),
                    out,
                    stdout,
                    collectible = true
                )
              
            let! globalConfig, localConfigs = Task.map2 tuple2 globalConfigTask localConfigTasks
            return AppState(sln, fsiSession, globalConfig, localConfigs, out)
        }
