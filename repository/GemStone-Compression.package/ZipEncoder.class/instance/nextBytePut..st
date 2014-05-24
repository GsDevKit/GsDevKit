accessing
nextBytePut: anObject 
	"Insert the argument at the next position in the Stream
	represented by the receiver."

	isBinary 
		ifTrue: [ self nextPut: anObject asInteger ]
		ifFalse: [self nextPut: anObject asCharacter ]