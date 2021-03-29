tests
testCannotLoadClass
	| d |
	d :=  self mockClass: 'MCMockClassT' super: 'Object' traitComposition: 'MCMockTraitA' classTraitComposition: 'MCMockTraitB'.
	self should: [d load] raise: Error.
	self deny: (Smalltalk hasClassNamed: 'MCMockClassT').
	
	d :=  self mockClass: 'MCMockClassT' super: 'Object' traitComposition: '{}' classTraitComposition: 'MCMockTraitB'.
	self should: [d load] raise: Error.
	self deny: (Smalltalk hasClassNamed: 'MCMockClassT').
	
	d :=  self mockClass: 'MCMockClassT' super: 'Object' traitComposition: 'MCMockTraitA' classTraitComposition: '{}'.
	self should: [d load] raise: Error.
	self deny: (Smalltalk hasClassNamed: 'MCMockClassT').