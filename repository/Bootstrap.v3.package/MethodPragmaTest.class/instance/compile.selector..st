utilities
compile: aString selector: aSelector
   | source errorString |
   [
    self class 
        compileMethod: (source := aSelector asString , String cr , aString)
        category: self methodCategory
        using: GsSession currentSession symbolList
   ] onException: CompileError do:[:ex |
     "Clear the pragmas for the method on a compiler error, colect error info for debugging"
     errorString := GsMethod _sourceWithErrors: ex errorDetails fromString: source.
     self class setPragmas: nil forMethod: aSelector
   ].
    ^self class compiledMethodAt: aSelector
