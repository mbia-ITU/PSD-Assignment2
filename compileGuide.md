# Lex and compile guide for fsl

## For Windows / emno

* use `fslex --unicode <file.fsl>` to generate a fs file
* wrap it in a F# project, where FsLexYacc.Runtime.dll is a dependency / reference
* use `dotnet build` to build the project.
* find the exe in the bin folder

## Windows using FSI

* use `dotnet fsi FsLexYacc.Runtime.dll <filename>.fs`, to open a interactive session with the files
* open the module
* run the main function from FSI


## For Mac

* `mono ~/bin/fsharp/fslex.exe --unicode <filename>.fsl`
* `fsharpc -r FsLexYacc.Runtime.dll <filename>.fs`
* `mono <filename>.exe`
