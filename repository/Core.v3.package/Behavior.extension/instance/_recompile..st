*core-squeak
_recompile: aSelector

    | source errorString |
    source := self sourceCodeAt: aSelector.
    [ | stamp |
      stamp := self stampForMethod: aSelector.
      self compileMethod: source
            category: (self categoryOfSelector: aSelector)
            using: GsSession currentSession symbolList  .
      self setStamp: stamp forMethod: aSelector
    ] onException: { CompileError . Warning } 
      do: { 
        [:ex | "handle CompileError" 
            errorString := GsMethod _sourceWithErrors: ex errorDetails fromString:  source.
        ].
        [:ex | "handle Warning"  ex resume ] 
      } .
    
    errorString ifNotNil:[ ^self error: errorString ].