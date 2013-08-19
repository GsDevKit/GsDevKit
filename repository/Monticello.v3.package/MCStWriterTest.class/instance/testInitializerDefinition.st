testing
testInitializerDefinition
	|chunk lastChunk|
	writer writeSnapshot: self mockSnapshot.
	stream reset.
	[stream atEnd] whileFalse:
		[chunk := stream nextChunk.
		chunk isAllSeparators ifFalse: [lastChunk := chunk]].
	self assertContentsOf: lastChunk readStream match: self expectedInitializerA