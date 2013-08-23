printing
printOn: aStream
	aStream
		nextPutAll: self class name;
		nextPut: $<;
		nextPutAll: self theClass name;
		nextPut: $#;
		nextPutAll: self selector;
		nextPut: $>