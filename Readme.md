### 🔥 FsiX 🔥

A better REPL for F# with project/solution support and hot reloading.  
Brings REPL driven development to F#!


### 📹 Demo


https://github.com/user-attachments/assets/fb14365c-758a-4603-b729-94e2eebe006d


---
## ✨ Features

- **Project & Solution Support** (`*.fsproj` / `*.sln` / `*.slnx`)  
  No need to manually reference DLLs or NuGets.  
  Thanks to [proj-info](https://github.com/ionide/proj-info/), FsiX automatically detects your project structure and loads everything for you.

- **Hot Reloading**  

  Redefine a function with the same name, type signature, and (optionally) module path —  
  FsiX will patch the original assembly so all calls use the new version instantly.

- **Inline Async & Computation Expressions**  

    Write code like:
  ```fsharp
  let! result = someAsyncCall
  ```
    and FsiX rewrites it into:
    ```fsharp
    let result = someAsyncCall |> _.Run()

    ```
    Perfect for step-by-step debugging of async and other computation expressions.

- **File imports with `#open`**

    Example:
    ```
    :open FileName.fs
    ```

    Opens the top-level module in MyFile.fs and any modules it depends on.
    Useful when working inside a specific file.
- **Shorter Directives**


    - `#q` instead of `#quit`.

    - `:q`, `:help`, and other `:` alternatives for directives.

        Use your vim muscle memory in F#!

- **Modern REPL Experience**

    Autocompletion, history, and more niceties via [PrettyPrompt](https://github.com/waf/PrettyPrompt)


## 🚀 Installation

If your project uses regular .NET SDK:
```
dotnet tool install --global fsix
```
Or, if it uses Asp net core sdk:
```
dotnet tool install --global fsix.web
```

Then, just run

```
dotnet fsix #or fsix-web
```
in the root dir of your project

### 🛠 Configuration

   There is configuration file in `~/.config/fsix/repl.fsx` if you are on GNU/Linux and `%appdata%\fsix\repl.fsx` if you are on windows.
   It's basically just an F# script which is being run on repl start, which has to contain `replConfig` object with prompt configuration.
   FsiX will also load on start `.repl.fsix` file if it's present in current dir.

### 📌 Future features
- fsix-daemon service which will use json-rpc protocol to communicate with any repl frontend
- vscode notebooks frontend
- ability to add REPL to any of your projects just as library


## 🤝 Contributing

Contributions are welcome!
Feel free to open issues or PRs.
