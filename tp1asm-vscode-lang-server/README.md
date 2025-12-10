# TP1 Assembly Language Server

## Functionality

This Language Server works for .tp1a files. It has the following language features:
- Diagnostics regenerated on each file change
- Syntax highlighting
- Completions for constants and registers
- Display value of constants on hover

## Setup

[vsce](https://code.visualstudio.com/api/working-with-extensions/publishing-extension) is required.

```bash
npm install
vsce package
```
Right click generated .vsix file in VSCode > Install Extension VSIX.

Then go to File > Preferences > Settings and set `tp1asmLanguageServer.assemblerExecutablePath` to the compiled assembler binary.

