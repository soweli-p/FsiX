module FsiX.Features.AutoCompletion

open System.Threading.Tasks
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Interactive
open FSharp.Compiler.Text
open FuzzySharp
open PrettyPrompt.Completion
open PrettyPrompt.Highlighting


open FSharpPlus

type CompletionItem = {
  DisplayText: string
  ReplacementText: string
  Kind: string
  GetFormattedDescription: (unit -> FormattedString option)
}

let scoreCandidate (enteredWord: string) (candidate: string) =
    seq {
        if candidate.StartsWith enteredWord then
            yield 200

        yield Fuzz.Ratio(enteredWord, candidate)
        yield int (100.0 / (float candidate.Length + 1.0))
    }
    |> Seq.sum

module FsCompletions =
  let mkDeclInfo (fsiSession: Shell.FsiEvaluationSession) text caret =
      let l = QuickParse.GetPartialLongNameEx(text, caret - 1)
      let parse, typed, _ = fsiSession.ParseAndCheckInteraction text
      let declList = typed.GetDeclarationListInfo(Some parse, 1, text, l)
      declList.Items

  let getFsCompletions (fsiSession: Shell.FsiEvaluationSession) text caret word =
      let declItems = mkDeclInfo fsiSession text caret

      let mkCompletionItem (declInfo: DeclarationListItem) =
          let getDocs () =
              let tagToColor =
                  function
                  | TextTag.Keyword -> AnsiColor.Blue
                  | TextTag.Function -> AnsiColor.Cyan
                  | _ -> AnsiColor.White

              let mkSpan (builder: FormattedStringBuilder) (tag: TaggedText) =
                  builder.Append(tag.Text, FormatSpan(0, tag.Text.Length, tagToColor tag.Tag))

              declInfo.Description
              |> (fun (ToolTipText elems) -> elems)
              |> Seq.collect (function
                  | ToolTipElement.Group e -> e
                  | _ -> [])
              |> Seq.tryHead
              |>> _.MainDescription
              |>> Seq.fold mkSpan (FormattedStringBuilder())
              |>> _.ToFormattedString()

          
          {
            CompletionItem.DisplayText = declInfo.NameInList
            ReplacementText = declInfo.NameInCode
            Kind = declInfo.Kind.ToString()
            GetFormattedDescription = getDocs
          }

      declItems
      |> Seq.map mkCompletionItem

module DirectiveCompletions =
    let directives =
        Set
            [ "reference"
              "include"
              "load"
              "time"
              "help"
              "clear"
              "quit"
              "open" ]

    open System.IO

    let mkReplacement wordToReplace entry =
        let maxVal = min (String.length entry) (String.length wordToReplace)
        let mutable i = 0

        while i < maxVal && wordToReplace[i] = entry[i] do
            i <- i + 1

        String.drop i entry

    let commandCompletions (text: string) carret (wordToReplace: string) = 
        if not <| String.contains ' ' text then
            directives
            |> Seq.map (fun keyword ->
              {
                CompletionItem.DisplayText = keyword
                ReplacementText = mkReplacement wordToReplace keyword
                Kind = "Keyword"
                GetFormattedDescription = konst None
              })
        else
            let currentWord =
                let textList = String.toList text

                let firstPart =
                    textList
                    |> Seq.rev
                    |> Seq.skip (text.Length - carret)
                    |> Seq.takeWhile (fun c -> c <> ' ')

                let lastPart = textList |> Seq.skip carret |> Seq.takeWhile (fun c -> c <> ' ')
                Seq.append (Seq.rev firstPart) lastPart |> String.ofSeq

            let isDirectory =
               text |> String.contains Path.DirectorySeparatorChar
               || text.StartsWith "#open"
               || text.StartsWith ":open"
            if isDirectory then
                let currentDir = [|Directory.GetCurrentDirectory(); Path.GetDirectoryName currentWord |] |> Path.Combine
                if Directory.Exists currentDir then
                    Directory.EnumerateFiles currentDir
                    |> Seq.append (Directory.EnumerateDirectories currentDir |> Seq.map (fun d -> d + "/"))
                    |> Seq.map (fun e -> Path.GetRelativePath(currentDir, e))
                    //|> Seq.map (mkReplacement currentWord)
                    |> Seq.map (fun fsEntry -> 
                      {
                        CompletionItem.DisplayText = fsEntry
                        ReplacementText = fsEntry
                        Kind = if fsEntry.EndsWith "/" then "Folder" else "File"
                        GetFormattedDescription = konst None
                      })
                else
                    []
            else
                []


let getCompletions session text carret word =
    let sortCompletions = 
      Seq.sortByDescending (fun c -> scoreCandidate word c.ReplacementText)
      >> Seq.truncate 50
      >> Seq.toList
    match String.tryHead text with
    | Some ':'
    | Some '#' -> DirectiveCompletions.commandCompletions text carret word |> sortCompletions
    | Some _ -> FsCompletions.getFsCompletions session text carret word |> sortCompletions
    | None -> []

