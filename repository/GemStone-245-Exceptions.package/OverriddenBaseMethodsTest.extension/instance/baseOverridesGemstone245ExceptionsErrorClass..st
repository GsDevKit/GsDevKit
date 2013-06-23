*gemstone-245-exceptions
baseOverridesGemstone245ExceptionsErrorClass: known
	self
		addTo: known
		forBehavior: Error class
		overrides: {
			#'handlesCategory:number:' -> 'handlesCategory: anObject number: anInteger

	(anObject == GemStoneError and: [anInteger // 1000 == 6]) ifTrue: [^false].
	^super handlesCategory: anObject number: anInteger.
'.
	}
