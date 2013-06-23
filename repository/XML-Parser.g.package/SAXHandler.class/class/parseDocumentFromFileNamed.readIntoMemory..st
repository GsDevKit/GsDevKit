instance creation
parseDocumentFromFileNamed: fileName readIntoMemory: readIntoMemory
	| stream xmlDoc |
	stream _ FileDirectory default readOnlyFileNamed: fileName.
	readIntoMemory
		ifTrue: [stream _ stream contentsOfEntireFile readStream].
	[xmlDoc _ self parseDocumentFrom: stream]
		ensure: [stream close].
	^xmlDoc