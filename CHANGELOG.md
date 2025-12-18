# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),

## [0.1.2] - 2025-12-18

### Changed

- Better working of cancellation token when evaluation is stuck.
- Fixed #open directive which didnt work for less than 2 files.
- Hot reloading middleware used to fail in some projects like Avalonia.
  - Issue is fixed if initialize it before start of interactive session, not after.
- fsix-daemon now uses tcp sockets instead of stdio:
  - If there was any output to stdout in separate thread, json-rpc connection used to break.
  - So now stdout is completely free and can be captured i.e for tracing.
  - By default, it assigns automatically any available port on localhost and prints it to stdout.
      - Or you can provide custom endpoint as first cli arg in form like: `127.0.0.1:9000`.
- You  can have fsix-daemon in regular cli tool too!
  - Just add `--daemon port addr` argument to `FsiX.Cli` or `FsiX.Cli.Web`

## [0.1.1] - 2025-12-05

### Changed

- Various fixes regarding new lines in configs and parsing.
- fsix-daemon released!
  - Uses json-rpc for communication.
  - Supports autocompletion and diagnostics, and all other fsix features.
  - Has separate config named `daemon.fsx`.
  - Is used in [fsix-vscode](https://github.com/soweli-p/fsix-vscode) (will be released soon).

### Breaking changes
- Even since [0.1.0], old packages `FsiX` and `FsiX.Web` are deprecated. 
    - Use new `FsiX.Cli` and `FsiX.Cli.Web` instead.
    - If having any conflicts, remove old tools from your environment.
- Config file is called `repl.fsx` now.


## [0.1.0] - 2025-11-22

### Changed

- Added .NET 10 support
- You can now opt in or opt out from having hot reloading or computation expression modifier!
    - These options can be found in `_fsiXHotReload: bool` and `_fsiXCompExpr` flags, which must be defined in your config.
- More modular architecture, including:
    - Most features are implemented like middlewares which modify request, reply and internal fsix state.
    - Abstracting away logging and any stdio.
    - Evaluation result and request are now serializable objects with additional info and metadata.
    - All of this done in preparation for fsix-daemon, which will use json-rpc to communicate with any repl frontend.


