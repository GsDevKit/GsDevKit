*ob-symbollistbrowser
isDescendantOfSymbolDictionary: aSymbolDictionaryNode

	aSymbolDictionaryNode symbolDictionary at: self theClass name asSymbol ifAbsent: [ ^false ].
	^true