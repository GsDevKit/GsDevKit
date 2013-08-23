*ob-standard-converting
asCategoryPrintString

	| stream cats sels |
	stream := WriteStream on: String new.

	cats := self _categories.
	self sortedCategoryNames do: [:catName |
		sels := cats at: catName.
		sels isEmpty 
			ifFalse: [
				stream nextPut: $(.
				stream nextPutAll: catName asString printString.
				sels asSortedCollection do: [:sel |
					stream space; nextPutAll: sel asString.
				].
				stream nextPut: $); cr.
			].
	].
	^stream contents