parsing
nextEntity
	| currentTop |
	currentTop _ self top.
	[self driver nextEntity isNil
		or: [self top ~~ currentTop]] whileTrue.
	^entity