writing xml
pcData: aString
	| lastIndex nextIndex |
	lastIndex := 1.
	"Unroll the first search to avoid copying"
	nextIndex := String findFirstInString: aString inSet: XMLTranslationMap startingAt: lastIndex.
	nextIndex = 0 ifTrue:[^self stream nextPutAll: aString].
	[self stream nextPutAll: (aString copyFrom: lastIndex to: nextIndex-1).
	self stream nextPutAll: (XMLTranslation at: (aString at: nextIndex)).
	lastIndex := nextIndex + 1.
	nextIndex := String findFirstInString: aString inSet: XMLTranslationMap startingAt: lastIndex.
	nextIndex = 0] whileFalse.
	self stream nextPutAll: (aString copyFrom: lastIndex to: aString size).