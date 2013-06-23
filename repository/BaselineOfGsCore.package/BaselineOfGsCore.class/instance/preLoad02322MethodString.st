doits
preLoad02322MethodString

	^'_compileMethod: source category: cat using: aSymbolList
	""
	| result errorString |
	result := self 
        compileMethod: source
        dictionaries: aSymbolList
        category: cat.
	result ~~ nil 
		ifTrue: [ | undefSymbol symbols undefinedSymbols undefinedSymbolList |
			undefSymbol := true.
			symbols := Array new.
			result do: [:errArray |
				(errArray at: 1) = 1031 
					ifTrue: [ 
						symbols add: (errArray at: 5) asSymbol.
						undefSymbol := UndefinedSymbolNotification signal]
					ifFalse: [undefSymbol := false]. 
			].
			undefSymbol
				ifTrue: [
					undefinedSymbols := aSymbolList objectNamed: #UndefinedSymbols.
					undefinedSymbols ~~ nil
						ifTrue: [
							symbols do: [:sym | | s |
								s := undefinedSymbols at: sym otherwise: nil.
								s == nil 
									ifTrue: [ 
										s := Set new.
										undefinedSymbols at: sym put: s
									].
							].
							undefinedSymbolList := aSymbolList objectNamed: #UndefinedSymbolList.
							result := self 
								compileMethod: source
								dictionaries: aSymbolList, undefinedSymbolList
								category: cat.
						]
						ifFalse: [ undefSymbol := false ].
				].
			result ~~ nil ifTrue: [ ^result ].
			undefSymbol ifTrue: [ symbols do: [:sym |  
				(undefinedSymbols at: sym) add: (self -> (Behavior parseSelector: source for: self)) ]]].
	^nil'