Adding
nextPut: anObject
"Inserts anObject as the next element that the receiver can access for writing.
 Returns anObject."

self isBinary
	ifTrue: [ 
		anObject isInteger
			ifFalse: [ ^self error: 'Expected an Integer' ]]
	ifFalse: [ 
		anObject isCharacter
			ifFalse: [ ^self error: 'Expected a Character' ]].
^ self gsfile nextPut: anObject