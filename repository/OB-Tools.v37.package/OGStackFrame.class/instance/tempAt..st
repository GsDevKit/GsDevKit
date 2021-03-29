accessing
tempAt: anInteger 

	frame size < (10 + anInteger) ifTrue: [^nil].
	^frame at: (10 + anInteger)