special cases
knownOverrides

	| knownOverrides dict |
	knownOverrides := IdentityDictionary new.
	(self class allSelectors select: [:ea | ea beginsWith: 'baseOverrides' ]) do:[:selector | 
		self perform: selector with: knownOverrides ].
	^knownOverrides 
