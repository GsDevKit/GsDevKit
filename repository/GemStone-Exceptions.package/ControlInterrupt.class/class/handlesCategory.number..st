ANSI - exceptionSignaler
handlesCategory: anObject number: anInteger

	^(anObject == GemStoneError or: [self handles: anObject]) and: [anInteger // 1000 = 6 or: [anInteger = 2318]].
