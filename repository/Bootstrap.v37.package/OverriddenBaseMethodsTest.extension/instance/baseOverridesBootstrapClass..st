*bootstrap
baseOverridesBootstrapClass: known
	"Class methods that get overridden when bootstrap mcz file is installed"
	
	self
		addTo: known
		forBehavior: Class
		overrides: {
			#'allSubclasses' -> 'allSubclasses

	^ClassOrganizer new allSubclassesOf: self.
'.
			#'subclasses' -> 'subclasses

	^ClassOrganizer new subclassesOf: self.
'.
			#'addSharedPool:' -> 'addSharedPool: aDictionary

"Add aDictionary to the end of the shared pool list for the receiver.

 You may use this method only if, when the receiver was created, the argument
 to poolDictionaries: was an Array (rather than a literal Array, which would
 create an InvariantArray).  See Class | subclass:."

self _validatePrivilege.
aDictionary _validateClass:  SymbolDictionary .
(poolDictionaries includesIdentical: aDictionary) "already in shared pool"
   ifTrue: [ ^ self _error: #classErrPoolDictExists args: #[aDictionary]].
poolDictionaries add: aDictionary
'.
			#'removeSharedPool:' -> 'removeSharedPool: aDictionary

"Remove aDictionary from the shared pool list for the receiver.  Generates an
 error if aDictionary is not a shared pool for the receiver.

 You may use this method only if, when the receiver was created, the argument
 to poolDictionaries: was an Array rather than a literal Array, which would
 create an InvariantArray.  (See Class | subclass:.)"

| index "loop index" |

self _validatePrivilege.
(poolDictionaries includesIdentical: aDictionary)
   ifTrue: [index := 1.  "find aDictionary in the pool"
            [aDictionary == (poolDictionaries at: index)]
               whileFalse: [index := index + 1].
            poolDictionaries deleteObjectAt: index]
   ifFalse: [ ^ self _error: #classErrPoolDictNotFound args: #[aDictionary]]
            "aDictionary not found in shared pool"
'.
	}