testing
hasLiteralSuchThat: aBlock
	^ (aBlock value: self keyword)
		or: [ self arguments hasLiteralSuchThat: aBlock ].