accessing
nextPutAll: aCollection
	aCollection do:[:ch| self nextPut: ch].
	^aCollection.