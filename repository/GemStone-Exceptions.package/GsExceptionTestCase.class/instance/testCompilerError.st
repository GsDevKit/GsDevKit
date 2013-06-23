tests
testCompilerError

	| caughtIt |
	caughtIt := false.
	[System 
		signal: 1031 "#StDBErrNotDefined" 
		args: {#StDBErrNotDefined. }
		signalDictionary: GemStoneError] on: CompilerError do: [:ex | caughtIt := true ].
	self assert: caughtIt.
