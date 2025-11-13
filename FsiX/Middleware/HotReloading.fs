module FsiX.Middleware.HotReloading
open System
open System.Reflection

open FsiX
open FsiX.ProjectLoading
open FsiX.Utils
open FsiX.AppState


//todo 
//load all methods, put them in hash by name
//also include their path when it's in modules
//and return type & args
//when user enters method with same name, type, try to reload it
//if it finds multiple, check one for last opened file
//otherwise give warming
//if none found with same type, give hint it's not possible to overload types
//otherwise print nothing

type Method = {
  MethodInfo: MethodInfo
  FullName: string
} with static member make modulePath (m : MethodInfo) = 
        { MethodInfo = m;
          FullName = m.Name :: modulePath |> Seq.rev |> String.concat "." }
type State = {
  Methods: Map<string, Method list>
  LastOpenModules: string list
  LastAssembly: Assembly Option
}

type Event = 
  | NewReplAssemblies of Assembly array
  | ModuleOpened of string

let getAllMethods (asm: Assembly) =
  let rec getMethods currentPath (t: Type) =
    let newPath =
      if t.Name.Contains "FSI_" then
        currentPath
      else 
        t.Name :: currentPath
    let methods = 
        t.GetMethods()
        |> Seq.filter (fun m -> m.IsStatic && not <| m.IsGenericMethod)
        |> Seq.map (Method.make newPath)
    let types = t.GetNestedTypes() |> Seq.toList
    methods 
    |> Seq.append (Seq.collect (getMethods (t.Name :: currentPath)) types)
  asm.GetExportedTypes() 
  |> Seq.collect (getMethods [])

open FSharpPlus
let mkReloadingState (sln: FsiX.ProjectLoading.Solution) = 
  let assemblies = 
    sln.Projects |> Seq.map (_.TargetPath >> Assembly.LoadFrom)
  let methods = 
    assemblies
    |> Seq.collect getAllMethods
    |> Seq.groupBy _.MethodInfo.Name
    |> Seq.map (fun (methodName, methods) -> methodName, List.ofSeq methods)
    |> Map.ofSeq
  {Methods = methods; LastOpenModules = []; LastAssembly = None}
  
let getReloadingState (st: AppState) =
  st.Custom
  |> Map.tryFind "hotReload"
  |> Option.map (fun reloadStObj -> reloadStObj :?> State)
  |> Option.defaultWith (fun () -> mkReloadingState st.Solution)


open HarmonyLib
let detourMethod (method: MethodBase) (replacement: MethodBase) = 
  typeof<Harmony>.Assembly 
  |> _.GetTypes()
  |> Seq.find (fun t -> t.Name = "PatchTools")
  |> fun x -> x.GetDeclaredMethods() 
  |> Seq.find (fun n -> n.Name = "DetourMethod") 
  |> fun x -> x.Invoke(null, [|method; replacement|])
  |> ignore

open FuzzySharp
let handleNewAsmFromRepl (logger: ILogger) (asm: Assembly) (st: State) = 
  match st.LastAssembly with 
  | Some prev when prev = asm -> st, []
  | _ ->
  let replacementPairs =
    getAllMethods asm
    |> Seq.choose (fun newMethod ->
        Map.tryFind newMethod.MethodInfo.Name st.Methods
        >>= (
          Seq.filter (fun existingMethod ->
            let getParams m = m.MethodInfo.GetParameters() |> Array.map _.ParameterType
            getParams existingMethod = getParams newMethod
            && existingMethod.MethodInfo.ReturnType = newMethod.MethodInfo.ReturnType
            && existingMethod.FullName.Contains newMethod.FullName
          ) 
          >> Seq.sortByDescending (fun existingMethod -> 
              let moduleCandidate = 
                st.LastOpenModules
                |> Seq.map (fun o -> Fuzz.Ratio(o + newMethod.FullName, existingMethod.FullName))
                |> Seq.tryHead
                |> Option.defaultValue 0
              let noModuleCandidate = Fuzz.Ratio(newMethod.FullName, existingMethod.FullName)
              max moduleCandidate noModuleCandidate
          )
          >> Seq.tryHead)
        |> Option.map (fun oldMethod -> oldMethod, newMethod)
      )
    |> Seq.toList
  for methodToReplace, newMethod in replacementPairs do
      logger.LogInfo <| "Updating method" + methodToReplace.FullName
      detourMethod methodToReplace.MethodInfo newMethod.MethodInfo
  {st with LastAssembly = Some asm}, List.map (snd >> _.FullName) replacementPairs


let getOpenModules replCode st = 
  let modules =  
    String.split [" "; "\n"] replCode
    |> Seq.filter ((<>) "")
    |> Seq.chunkBySize 2
    |> Seq.filter (Array.tryHead >> Option.map ((=) "open") >> Option.defaultValue false)
    |> Seq.map (fun arr -> arr[1])
    |> Seq.toList
  {st with LastOpenModules = (modules @ st.LastOpenModules) |> List.distinct}
  
let hotReloadingMiddleware next (request, st: AppState) =
  let hotReloadFlagEnabled =
    match st.Session.TryFindBoundValue "_fsiXHotReload" with
    | Some fsiBoundValue when fsiBoundValue.Value.ReflectionValue = true -> true
    | _ -> false
  let shouldRunHotReload (m: Map<string, obj>) =
    match hotReloadFlagEnabled, Map.tryFind "hotReload" m with
    | _, Some v when v = true -> true
    | true, None -> true
    | _ -> false
    
  match request with
  | {Args = m} when shouldRunHotReload m ->
      let response, st = next (request, st)
      if response.Error.IsSome then response, st
      else
      let asm = st.Session.DynamicAssemblies |> Array.last
      let reloadingSt, updatedMethods =
        getReloadingState st
        |> getOpenModules response.EvaluatedCode
        |> handleNewAsmFromRepl st.Logger asm
      {response with Metadata = response.Metadata.Add("reloadedMethods", updatedMethods)},
      {st with Custom = st.Custom.Add("hotReload", reloadingSt)}
  | _ -> next (request, st)
