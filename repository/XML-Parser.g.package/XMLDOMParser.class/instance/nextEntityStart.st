parsing
nextEntityStart
	[self driver nextEntity.
	self stack isEmpty] whileTrue.
	^entity