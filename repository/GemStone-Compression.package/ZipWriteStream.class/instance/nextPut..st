accessing
nextPut: anObject

	position <= itsCollection size
		ifTrue: [ ^ super nextPut: anObject ].
	^self pastEndPut: anObject