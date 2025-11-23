module FsiX.SaveRestore

open System
open System.IO
open System.Text

open FsiX.AppState

let private serialize (history: string seq) =
    history
    |> Seq.map (Encoding.UTF8.GetBytes >> Convert.ToBase64String)
    |> String.concat "\n"

let private deserialize lines =
    try
        lines
        |> Seq.map (Convert.FromBase64String >> Encoding.UTF8.GetChars >> String)
        |> Seq.toList
        |> Some
    with :? FormatException -> None

let save st path =
    try
        st.EvalHistory
        |> List.rev
        |> serialize
        |> fun s -> File.WriteAllText(path, s)
        true
    with :? IOException -> false

let restore st path =
    try
        path
        |> File.ReadAllLines
        |> deserialize
        |> function
        | Some history ->
            for code in history do
                st.Session.EvalInteraction(code)
            {st with EvalHistory = List.rev history @ st.EvalHistory}
            |> Some
        | None -> None
    with :? IOException -> None
