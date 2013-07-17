modifying
addCoreMethod: aMethodReference
	| category |
	category _ self baseCategoryOfMethod: aMethodReference.
	aMethodReference actualClass organization
		classify: aMethodReference methodSymbol
		under: category
		suppressIfDefault: false