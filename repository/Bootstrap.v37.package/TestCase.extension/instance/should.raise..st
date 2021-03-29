*bootstrap
should: aBlock raise: anObject

	| expectedErrorNumber actualErrorNumber |
	(anObject isKindOf: Integer) ifTrue: [
		expectedErrorNumber := anObject.
	] ifFalse: [(anObject isKindOf: Symbol) ifTrue: [
		expectedErrorNumber := ErrorSymbols at: anObject.
	] ifFalse: [
		^self assert: (self executeShould: aBlock inScopeOf: anObject).
	]].
	aBlock on: Error do: [:ex | 
	   actualErrorNumber := ex number.
	].
	self should: [actualErrorNumber = expectedErrorNumber].