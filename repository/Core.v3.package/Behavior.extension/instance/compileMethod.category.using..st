*core-squeak
compileMethod: source category: cat using: aSymbolList

  "Returns receiver, or signals a CompileError"

  ^self compileMethod: source category: cat using: aSymbolList environmentId: 0
