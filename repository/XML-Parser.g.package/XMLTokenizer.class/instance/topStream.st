streaming
topStream
	^self hasNestedStreams
		ifTrue: [self nestedStreams first]
		ifFalse: [self stream]