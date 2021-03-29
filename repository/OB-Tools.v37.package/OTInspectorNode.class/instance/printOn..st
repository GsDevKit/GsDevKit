printing
printOn: aStream
	super printOn: aStream.
	aStream nextPutAll: ' value: '; print: self value