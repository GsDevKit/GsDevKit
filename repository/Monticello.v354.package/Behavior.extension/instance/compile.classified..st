*monticello
compile: methodText classified: cat
   "Returns nil, or a selector Symbol, or signals a CompileError"

    self compileMethod: methodText
        category: cat
        using: GsSession currentSession symbolList.

    ^ (Behavior parseSelector: methodText for: self) ifNotNil:[ :s | s asSymbol]
