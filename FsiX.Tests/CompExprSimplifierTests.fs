module FsiX.Tests.CompExprSimplifierTests

open Expecto

open FsiX.Middleware.ComputationExpression
open FsiX.Utils

let isCompExpr =
    parse
    >> Async.RunSynchronously
    >> Option.map (fun res -> res.tree |> isCompExpr)
    >> Option.defaultValue false

type MockLogger() =
    interface ILogger with
        member this.LogDebug _ = ()
        member this.LogInfo _ = ()
        member this.LogError _ = ()
        member this.LogWarning _ = ()
let mockLogger = MockLogger()

let rewriteExpr = rewriteCompExpr mockLogger >> Async.RunSynchronously >> _.Replace("\r\n", "\n")


[<Tests>]
let tests =
    testList
        "comp expr tests"
        [ testCase "test let no bang"
          <| fun _ -> Expect.isFalse (isCompExpr "let a = 10") "let a = 10 - no comp expr"
          testCase "test let bang"
          <| fun _ -> Expect.isTrue (isCompExpr "let! a = 10") "let! a = 10 - comp expr"
          testCase "test let bang tab"
          <| fun _ -> Expect.isTrue (isCompExpr "   let! a = 10") "let! a = 10 - comp expr"
          testCase "test let bang multiline"
          <| fun _ -> Expect.isTrue (isCompExpr "let a = 10\nlet! b = 20") "let a =10\nlet! b = 20 - comp expr"
          testCase "test if bang"
          <| fun _ ->
              let code =
                  """
    if true then 
      let! a = 10
      return a
    else
      return 0
    """

              Expect.isTrue (isCompExpr code) "if else with comp expr"
          testCase "test if bang reverse"
          <| fun _ ->
              let code =
                  """
      if true then 
        return a
      else
        let! a = 10
        return 0
      """

              Expect.isTrue (isCompExpr code) "if else with comp expr"

          testCase "test bang rewrite"
          <| fun _ ->
              let code = "let! a = 10"
              Expect.equal (rewriteExpr code) "let a = (10).Run()\n" "let bang rewrite"
          testCase "test bang rewrite tab"
          <| fun _ ->
              let code = "    let! a = 10"
              Expect.equal (rewriteExpr code) "let a = (10).Run()\n" "let bang rewrite"
          testCase "test bang rewrite multiline"
          <| fun _ ->
              let code = "let a = 10\nlet! b = 20"
              Expect.equal (rewriteExpr code) "let a = 10\n\nlet b = (20).Run()\n" "let bang rewrite"

          testCase "test bang rewrite multiline expr"
          <| fun _ ->
              let code =
                  """
        let! a =
          someComplex
          |>> someMap
          |> multiline
              
        """

              let exp = "let a = (someComplex |>> someMap |> multiline).Run()\n\n\n"
              Expect.equal (rewriteExpr code) exp "let bang rewrite"
          testCase "test non let rewrite "
          <| fun _ ->
              let code =
                  """
      let! a =
        someComplex
        |>> someMap
        |> multiline
      return! a
            
      """

              let exp =
                  """let a = (someComplex |>> someMap |> multiline).Run()
(a).Run()"""

              Expect.equal (rewriteExpr code) exp "let bang rewrite"
          testCase "test if else"
          <| fun _ ->
              let code =
                  """
      let! a =
        someComplex
        |>> someMap
        |> multiline
      if a then 
        let! c = 200
        do! sasa
      else
        let! f = 200
        do! baba
            
      """

              let exp =
                  """let a = (someComplex |>> someMap |> multiline).Run()

if a then
    let c = (200).Run()
    do (sasa).Run()
else
    let f = (200).Run()
    do (baba).Run()"""

              Expect.equal (rewriteExpr code) exp "let bang rewrite" ]
