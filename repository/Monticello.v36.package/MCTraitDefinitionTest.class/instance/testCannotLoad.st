tests
testCannotLoad
	| d |
	d :=  self mockTrait: 'MCMockTraitA'.
	self should: [d load] raise: Error.
	self deny: (Smalltalk hasClassNamed: 'MCMockTraitA').