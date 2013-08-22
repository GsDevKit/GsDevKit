printing
printOn: aStream
	aStream
		print: self class;
		nextPut: $<;
		print: self theClass;
		nextPut: $>