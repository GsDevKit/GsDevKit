instance creation
parserOnFileNamed: fileName readIntoMemory: readIntoMemory
	| stream  |
	stream _ FileDirectory default readOnlyFileNamed: fileName.
	readIntoMemory
		ifTrue: [stream _ stream contentsOfEntireFile readStream].
	^self on: stream