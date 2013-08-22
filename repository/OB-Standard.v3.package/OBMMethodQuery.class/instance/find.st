executing
find
	isWild ifTrue: [self findMethod] ifFalse: [self browseImplementors]