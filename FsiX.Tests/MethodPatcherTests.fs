module FsiX.Tests.MethodPatcherTests

open Expecto
open System.Reflection
open System

module TestMethods =
  let methodToPatch (m: String) = sprintf "shiny %s" m
  let patchedMethod (m: String) = sprintf "patched %s" m
  let methodToTest m = 
    methodToPatch m + methodToPatch m


let patched, toPatch =
  let t = 
    Assembly.GetExecutingAssembly()
    |> _.GetTypes()
    |> Seq.find (fun t -> t.Name.Contains "TestMethods")
  t.GetMethod("patchedMethod"), t.GetMethod("methodToPatch")

open HarmonyLib
let detourMethod (method: MethodBase) (replacement: MethodBase) = 
  typeof<Harmony>.Assembly 
  |> _.GetTypes()
  |> Seq.find (fun t -> t.Name = "PatchTools")
  |> fun x -> x.GetDeclaredMethods() 
  |> Seq.find (fun n -> n.Name = "DetourMethod") 
  |> fun x -> x.Invoke(null, [|method; replacement|])
  
[<Tests>]
let tests =

  testList "method patcher tests"
      [ testCase "test method data"
        <| fun _ -> Expect.equal patched.ReturnType toPatch.ReturnType "return type equal"
        testCase "before patch" 
        <| fun _ -> 
          let isOld = TestMethods.methodToTest "" |> fun s -> s.Contains "shiny"
          Expect.isTrue isOld "is old method"
        testCase "after patch" 
        <| fun _ -> 
          detourMethod toPatch patched
          let isNew = TestMethods.methodToTest "" |> fun s -> s.Contains "patched"
          Expect.isTrue isNew "is new method"
          Expect.equal (TestMethods.methodToPatch "h") "patched h" "patched"
      ]

open FSharp.Compiler.Interactive.Shell
open FSharp.Compiler.Tokenization
open System
open System.IO
open System.Reflection
[<Tests>]
let fsiTests =

  // Initialize output and input streams
  let inStream = new StreamReader(Stream.Null)
  let outStream = Console.Out
  let errStream = Console.Out
  let fsiConfig =
      FsiEvaluationSession.GetDefaultConfiguration()

  let mkFsi () = FsiEvaluationSession.Create(fsiConfig, [|"fsi.exe"|], inStream, outStream, errStream)
  testList "fsiTests"
      [ 
     // testCase "test fsi1 "
     // <| fun _ -> 
     //   let fsi1 = mkFsi ()
     //   fsi1.EvalInteraction "let fff = 104"
     //   fsi1.EvalInteraction "let someFunc a b = a * b"
     //   Expect.isTrue true ""
        testCase "test fsi2 "
          <| fun _ -> 
            let fsi1 = mkFsi ()
            fsi1.EvalInteraction "let fff = 104"
            fsi1.EvalInteraction "let someFunc a b = a * b"
            let fsi2 = mkFsi ()
            let fsiCompiler = typeof<FsiEvaluationSession>.GetField("fsiDynamicCompiler", BindingFlags.NonPublic ||| BindingFlags.Instance).GetValue(fsi2)
            let assemblies = 
              fsiCompiler.GetType().GetField("dynamicAssemblies", BindingFlags.NonPublic ||| BindingFlags.Instance).GetValue(fsiCompiler)
              :?> ResizeArray<Assembly>
            let fields =typeof<FsiEvaluationSession>.GetDeclaredFields()
            for f in fields do
              printfn "field %s" f.Name

            assemblies.AddRange(fsi1.DynamicAssemblies)
            fsi2.EvalInteraction("open FSI_0001")
            fsi2.EvalInteraction("open FSI_0002")
            //for r in fsi1.DynamicAssemblies |> Seq.collect (_.DefinedTypes) do
              //fsi2.EvalInteraction("#open ")
              //r.dynamicAssemblies
              //printfn "having asm %s" (r.FullName)

            for bv in fsi1.GetBoundValues() do
              printfn "adding %s" (bv.Value.ToString())
              fsi2.AddBoundValue (bv.Name, bv.Value.ReflectionValue)
            fsi2.EvalInteraction("fff")
            fsi2.EvalInteraction("someFunc fff 2")
            Expect.isTrue true ""
      ]
