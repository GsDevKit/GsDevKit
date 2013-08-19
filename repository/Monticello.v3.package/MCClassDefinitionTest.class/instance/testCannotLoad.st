as yet unclassified
testCannotLoad
	| d |
	d :=  self mockClass: 'MCMockClassC' super: 'NotAnObject'.
	self should: [d load] raise: Error.
	self deny: (Smalltalk hasClassNamed: 'MCMockClassC').