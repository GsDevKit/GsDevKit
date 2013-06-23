*bootstrap
removeSharedPool: aDictionaryOrSharedPool
	"Remove aDictionary from the shared pool list for the receiver.  Generates an
	 error if aDictionary is not a shared pool for the receiver.

	 You may use this method only if, when the receiver was created, the argument
	 to poolDictionaries: was an Array rather than a literal Array, which would
	 create an InvariantArray.  (See Class | subclass:.)"

	| pd index "loop index" |

	self _validatePrivilege.
	pd := aDictionaryOrSharedPool.
	((aDictionaryOrSharedPool isKindOf: Class) and: [aDictionaryOrSharedPool isSubclassOf: SharedPool])
		ifTrue: [ pd := aDictionaryOrSharedPool _classVars ].
	(poolDictionaries includesIdentical: pd)
   		ifTrue: [index := 1.  "find aDictionary in the pool"
            	[pd == (poolDictionaries at: index)]
               	whileFalse: [index := index + 1].
            	poolDictionaries deleteObjectAt: index]
   		ifFalse: [ ^ self _error: #classErrPoolDictNotFound args: #[pd]]
            	"aDictionary not found in shared pool"
