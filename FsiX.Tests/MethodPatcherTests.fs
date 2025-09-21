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
