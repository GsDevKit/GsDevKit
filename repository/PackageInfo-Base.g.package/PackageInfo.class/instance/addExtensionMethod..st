modifying
addExtensionMethod: aMethodReference
	| category |
	category _ self baseCategoryOfMethod: aMethodReference.
	aMethodReference actualClass 
		classify: aMethodReference methodSymbol
		under: self methodCategoryPrefix, '-', category