*ob-standard-categories
changeFromString: aString 
	"Parse the argument, aString, and make this be the receiver's structure."

	| categorySpecs |
	categorySpecs _ Scanner new scanTokens: aString.
	"If nothing was scanned and I had no elements before, then default me"
	categorySpecs isEmpty
		ifTrue: [^ self].

	^ self changeFromCategorySpecs: categorySpecs