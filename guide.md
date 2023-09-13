For Mac
mono ~/bin/fsharp/fslex.exe --unicode <filename>.fsl
fsharpc -r FsLexYacc.Runtime.dll <filename>.fs
mono <filename>.exe