configuration
defaultMetaNode
	| symbolList symbolDictionary |
	symbolList := OBMetaNode named: 'SymbolList'.
	symbolDictionary := OBMetaNode named: 'SymbolDictionary'.
	symbolList childAt: #symbolDictionaries put: symbolDictionary.
	symbolDictionary ancestrySelector: #isDescendantOfSymbolDictionary:.
	self buildMetagraphOn: symbolDictionary.
	^symbolList