# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),

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


