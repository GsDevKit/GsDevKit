*core-squeak-subclass creation
_resolveUndeclaredSymbolsFor: aSymbol
	| undefinedSymbols |
	undefinedSymbols := GsSession currentSession symbolList objectNamed: #UndefinedSymbols.
	undefinedSymbols ~~ nil
		ifTrue: [
			(undefinedSymbols at: aSymbol otherwise: Set new) do: [:assoc | 
				(assoc key includesSelector: assoc value) 
					ifTrue: [ assoc key _recompile: assoc value ]].
			(undefinedSymbols at: aSymbol otherwise: Set new) isEmpty 
				ifTrue: [ undefinedSymbols removeKey: aSymbol ifAbsent: []]].