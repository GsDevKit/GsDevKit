accessing
printOn: aStream
	super printOn: aStream.
	aStream nextPut: $<.
	self name printOn: aStream.
	aStream nextPut: $>.