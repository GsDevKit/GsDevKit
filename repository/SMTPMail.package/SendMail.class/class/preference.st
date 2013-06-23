preferences
preference
	^Preference isNil
		ifTrue: [Preference := SymbolDictionary new]
		ifFalse: [Preference]
		