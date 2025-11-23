module FsiX.ProjectLoading

open System
open System.IO

open FSharp.Compiler.CodeAnalysis
open Ionide.ProjInfo

open Ionide.ProjInfo.Types
open FsiX.Utils
open FsiX.Args



type FileName = string
type DllName = string
type DirName = string

type Solution =
    { FsProjects: FSharpProjectOptions list
      Projects: ProjectOptions list
      StartupFiles: FileName list
      References: DllName list
      LibPaths: DirName list
      OtherArgs: string list
    }

let loadSolution (logger: ILogger) (args: Arguments list) =
    let directory =
          args
          |> List.tryPick (function | Dir d -> Some d | _ -> None) 
          |> Option.defaultWith Directory.GetCurrentDirectory
    let solutions =
      match args |> List.choose (function | Sln s -> Some s | _ -> None) with
      | [] -> Directory.EnumerateFiles directory |> Seq.filter (fun s -> s.EndsWith ".sln") |> Seq.toList
      | s -> s |> List.map Path.GetFullPath
    let projects =
      match args |> List.choose (function | Proj p -> Some p | _ -> None) with
      | [] -> Directory.EnumerateFiles directory |> Seq.filter (fun s -> s.EndsWith ".fsproj") |> Seq.toList
      | s -> s |> List.map Path.GetFullPath
    match solutions, projects with
    | [], [] ->
      logger.LogWarning "Couldnt find any solution or project"
      { FsProjects = []; Projects = []; StartupFiles = [];
        References = []; LibPaths = []; OtherArgs = []
      }
    | _ ->
    
    let toolsPath = Init.init (DirectoryInfo directory) None
    let defaultLoader: IWorkspaceLoader = WorkspaceLoader.Create(toolsPath, [])

    let projects =
      solutions 
      |> Seq.collect defaultLoader.LoadSln
      |> Seq.append (defaultLoader.LoadProjects projects)

    let fcsProjectOptions = List.ofSeq <| FCS.mapManyOptions projects

    let startupFiles =
      args |> List.choose (function | Use f -> Some (Path.GetFullPath f) | _ -> None )
    let references =
      args |> List.choose (function | Reference r -> Some (Path.GetFullPath r) | _ -> None )
    let libPaths =
      args |> List.collect (function | Lib l -> List.map Path.GetFullPath l | _ -> [] )
    let otherArgs = args |> List.collect (function | Other args -> args | _ -> [] )

    { FsProjects = fcsProjectOptions
      Projects = projects |> Seq.toList
      StartupFiles = startupFiles
      References = references
      LibPaths = libPaths
      OtherArgs = otherArgs
    }


let solutionToFsiArgs (logger: ILogger) useAsp sln =
  let projectDlls = sln.Projects |> Seq.map _.TargetPath 
  let nugetDlls = 
    sln.Projects
    |> Seq.collect _.PackageReferences
    |> Seq.map _.FullPath
  let otherDlls = sln.References
  let allDlls = 
    projectDlls
    |> Seq.append nugetDlls
    |> Seq.append otherDlls
    |> Seq.distinct
    |> List.ofSeq
  if List.exists (File.Exists >> not) allDlls then
    logger.LogError "Not all dlls are found! Pleaase build your project before running REPL"
    Environment.Exit 1
  [|
    "fsi"
    yield! allDlls |> Seq.map (sprintf "-r:%s")
    yield! sln.LibPaths |> Seq.map (sprintf "--lib:%s")
    yield! sln.OtherArgs
    if useAsp then
      yield!
        sln.Projects 
        |> Seq.collect _.OtherOptions 
        |> Seq.filter (fun s -> s.StartsWith "-r" && s.EndsWith ".dll" && s.Contains "Asp")
  |]


