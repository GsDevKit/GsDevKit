printing
printOn: aStream 
	aStream
		nextPutAll: self className;
		nextPutAll: '<'.
	self index printOn: aStream.
	aStream nextPut: $>