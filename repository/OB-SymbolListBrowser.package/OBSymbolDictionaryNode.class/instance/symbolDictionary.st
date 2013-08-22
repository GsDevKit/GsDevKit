accessing
symbolDictionary

	symbolDictionary == nil ifTrue: [ symbolDictionary := self getSymbolDictionary ].
	^symbolDictionary