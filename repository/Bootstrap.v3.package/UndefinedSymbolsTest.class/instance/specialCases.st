special cases
specialCases

	| undefinedSymbols |
	undefinedSymbols := Set new.
	(self class allSelectors select: [:ea | ea beginsWith: 'undefinedSymbols' ]) do:[:selector | undefinedSymbols addAll: (self perform: selector) ].
	^undefinedSymbols 
