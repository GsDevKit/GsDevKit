testing
hasLiteral: aLiteral
	^ self keyword == aLiteral 
		or: [ self arguments hasLiteral: aLiteral ].