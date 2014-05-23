*core-squeak
_compileMethod: source category: cat using: aSymbolList environmentId: anEnvironmentId
  "Attempts auto-recompile for undefinedSymbols. 
   Returns self or signals a CompileError"
  [ self compileMethod: source
          dictionaries: aSymbolList
          category: cat
    environmentId: anEnvironmentId.
  ] onSynchronous: { CompileError. CompileWarning. } do:
	{ 
	[:ex | 
    		| undefSymbol symbols undefinedSymbols undefinedSymbolList |
    		undefSymbol := true.
    		symbols := { } .
    		ex errorDetails do:[ :errArray |
      		(errArray atOrNil: 1) == 1031 ifTrue: [ symbols add: (errArray atOrNil: 5) asSymbol]
       			  	   ifFalse: [undefSymbol := false]. 
    		].
    		undefSymbol ifTrue:[
      		(UndefinedSymbolNotification new args: ex gsArguments; signal) ifTrue:[
        		"attempt auto-define of undefined symbols"
        		(undefinedSymbols := aSymbolList objectNamed: #UndefinedSymbols) ifNotNil:[
          		symbols do: [:sym | | s |
            		(s := undefinedSymbols at: sym otherwise: nil) ifNil:[
              		s := Set new.
              		undefinedSymbols at: sym put: s
            		].
          		].
         		 undefinedSymbolList := aSymbolList objectNamed: #UndefinedSymbolList.
          		[
            		self compileMethod: source dictionaries: aSymbolList, undefinedSymbolList
                   		category: cat environmentId: anEnvironmentId.
          		] onException: CompileError do:[ :exb |  
            		undefSymbol := false
          		]
        		].
      		] ifFalse:[ undefSymbol := false ].
    		].
    		undefSymbol ifTrue: [   "recompile ok, add to undefinedSymbols dict"
      		symbols do: [:sym |  
			(undefinedSymbols at: sym) add: (self -> (Behavior parseSelector: source for: self)) 
      		]
    		] ifFalse:[ ex outer ]]. 
	[:ex | ex resume ]
	}.
  	^ self