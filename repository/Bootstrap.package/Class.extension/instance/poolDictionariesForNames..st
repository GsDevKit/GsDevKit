*bootstrap
poolDictionariesForNames: poolDictionaryNames

	| ar existingDict |
	ar := Array new.
	poolDictionaryNames do: [:poolName |
		existingDict := GsSession currentSession userProfile symbolList objectNamed: poolName.
		(existingDict isKindOf: SymbolDictionary)
			ifTrue: [ ar add: existingDict ].
		((existingDict isKindOf: Class) and: [existingDict isSubclassOf: SharedPool])
			ifTrue: [ 
				ar add: existingDict _classVars.
				existingDict _classVars name: poolName asSymbol.
			].
		existingDict == nil
			ifTrue: [ | pool |
				pool := SymbolDictionary new.
				pool name: poolName asSymbol.
				ar add: pool.
			].
	].
^ar