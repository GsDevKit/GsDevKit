*ob-gemstone-platform-GSExtensions
findMethodNamesMatching: aString
	| matches methodNames |
	matches := Set new.
	((AllUsers userWithId: 'SymbolUser') resolveSymbol: #AllSymbols) value 
		keysDo: [ :symbol | 
			(symbol isValidSelector and: [symbol includesString: aString])
				ifTrue: [matches add: (NameMatch new name: symbol matching: aString)]].
	^self sortNameMatchesAsString: matches
	