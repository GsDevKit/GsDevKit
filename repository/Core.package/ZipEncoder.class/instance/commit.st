initialize-release
commit
	encodedStream nextPutAll: (itsCollection copyFrom: 1 to: position - 1).
	position :=  1.