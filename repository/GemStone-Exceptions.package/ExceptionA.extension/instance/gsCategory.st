*gemstone-exceptions
gsCategory

	gsCategory notNil ifTrue: [^gsCategory].
	gsCategory := self class exceptionCategory.
	(gsCategory isKindOf: SymbolDictionary) ifFalse: [gsCategory := gsCategory new].
	^gsCategory.
