accessing
getSymbolDictionary

	self symbolList do: [:symbolDict | symbolDict name == self name ifTrue: [ ^symbolDict ]].
	^nil