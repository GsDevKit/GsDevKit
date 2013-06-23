*bootstrap
_addUndefinedSymbol: undefindedSymbol to: undefinedSymbols for: aMethod

	| s |
	s := undefinedSymbols at: undefindedSymbol otherwise: nil.
	s == nil 
		ifTrue: [ 
			s := Set new.
			undefinedSymbols at: undefindedSymbol put: s ].
	s add: (aMethod inClass -> aMethod selector)
	