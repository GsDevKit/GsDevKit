accessing
packagesHighlighted

	| result packageDict |
	versions == nil ifTrue: [ self refresh ].
	packageDictionary == nil
		ifTrue: [
			packageDict := Dictionary new.
			versions do: [:ar |
				(packageDict at: ar first ifAbsentPut:
					[(MCPackageNode name: ar first repository: repository)
						repositoryNode: self;
						packageName: ar last packageName;
						yourself])
						addVersion: ar last].	
			packageDictionary := packageDict ].
	result := packageDictionary values.
	"sort loaded packages first, then alphabetically"
	result := result asSortedCollection: [:a :b |
		| loadedA loadedB |
		loadedA := loaded anySatisfy: [:each | (each copyUpToLast: $-) = a name asString ].
		loadedB := loaded anySatisfy: [:each | (each copyUpToLast: $-) = b name asString ].
		loadedA = loadedB 
			ifTrue: [a name asString < b name asString]
			ifFalse: [loadedA]].

	^result collect: [:each | each name: (self packageHighlight: each name asString)]