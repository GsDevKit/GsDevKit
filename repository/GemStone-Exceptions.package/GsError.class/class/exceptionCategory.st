ANSI support
exceptionCategory

	^ExceptionA hasFixFor39741
		ifTrue: [self new]
		ifFalse: [GemStoneError].
