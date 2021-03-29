*bootstrap
addSharedPool: aDictionaryOrSharedPool
	"Add aDictionary to the end of the shared pool list for the receiver.

	 You may use this method only if, when the receiver was created, the argument
	 to poolDictionaries: was an Array (rather than a literal Array, which would
	 create an InvariantArray).  See Class | subclass:."

	self _validatePrivilege.
	((aDictionaryOrSharedPool isKindOf: Class) and: [aDictionaryOrSharedPool isSubclassOf: SharedPool])
		ifTrue: [ | pd |
			pd := aDictionaryOrSharedPool _createClassVarsDict.
			pd _validateClass:  SymbolDictionary .
			(poolDictionaries includesIdentical: pd) "already in shared pool"
   				ifFalse: [ 
					poolDictionaries add: pd.
					aDictionaryOrSharedPool _classVars name: aDictionaryOrSharedPool name asSymbol ]]
		ifFalse: [
			aDictionaryOrSharedPool _validateClass:  SymbolDictionary .
			(poolDictionaries includesIdentical: aDictionaryOrSharedPool) "already in shared pool"
   				ifTrue: [ ^ self _error: #classErrPoolDictExists args: { aDictionaryOrSharedPool }].
			poolDictionaries add: aDictionaryOrSharedPool ]