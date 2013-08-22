printing
printOn: aStream
	aStream
		nextPutAll: 'Fan<'.
	self parent name printOn: aStream.
	aStream nextPutAll: ', '.
	self children size printOn: aStream.
	aStream
		nextPut: $>