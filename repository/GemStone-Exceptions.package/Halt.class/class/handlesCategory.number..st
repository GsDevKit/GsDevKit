ANSI - exceptionSignaler
handlesCategory: anObject number: anInteger

	^(anObject == GemStoneError or: [self handles: anObject]) 
		and: [(anInteger == self errorNumberToCatch) or: [anInteger == Pause errorNumberToCatch ] ].
