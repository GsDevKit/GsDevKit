tests
testClassVarNonOrdering

	| d d2 c e x |
	d :=  self mockClass: 'MCMockClassC' super: 'Object' classVarNames: #(  B A C D ).
	d2 :=  self mockClass: 'MCMockClassC' super: 'Object' classVarNames: #( C B D A ).
	
	self assert: d2 equals: d.
	
	" test that after loading d it is still the same as d2"
	MCPlatformSupport suspendAutoCommitAndMigrationDuring: [ d load ].
	self assert: (Smalltalk hasClassNamed: 'MCMockClassC').
	c := (Smalltalk classNamed: 'MCMockClassC').

	self assert: c asUncachedClassDefinition equals: d.
	self assert: c asUncachedClassDefinition equals: d2.

	MCPlatformSupport suspendAutoCommitAndMigrationDuring: [ d unload ].
	self deny: (Smalltalk hasClassNamed: 'MCMockClassC').
	
	" test that after loading d2 it is still the same as d"
	MCPlatformSupport suspendAutoCommitAndMigrationDuring: [ d2 load ].
	self assert: (Smalltalk hasClassNamed: 'MCMockClassC').
	c := (Smalltalk classNamed: 'MCMockClassC').

	self assert: c asUncachedClassDefinition equals: d2.
	self assert: c asUncachedClassDefinition equals: d.

	MCPlatformSupport suspendAutoCommitAndMigrationDuring: [ d2 unload ].
	self deny: (Smalltalk hasClassNamed: 'MCMockClassC').