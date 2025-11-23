module FsiX.Utils

//todo make instances
type ILogger =
  abstract member LogInfo: string -> unit
  abstract member LogDebug: string -> unit
  abstract member LogWarning: string -> unit
  abstract member LogError: string -> unit


module Configuration = 
  open System
  open System.IO
  open System.Reflection

  let getEmbeddedFileAsString fileName = task {
    let asm = Assembly.GetExecutingAssembly()
    use stream = asm.GetManifestResourceStream fileName
    use reader = new StreamReader(stream)
    return! reader.ReadToEndAsync()
  }
  let getBaseConfigString () = getEmbeddedFileAsString "FsiX.base.fsx"
  let getConfigDir () = 
    let configDir =
        Environment.GetFolderPath Environment.SpecialFolder.ApplicationData
        |> fun s -> Path.Combine [| s; "fsix"; |]
    if not <| Directory.Exists configDir then do
      Directory.CreateDirectory configDir |> ignore
    configDir
