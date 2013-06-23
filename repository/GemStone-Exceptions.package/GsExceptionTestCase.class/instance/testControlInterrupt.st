tests
testControlInterrupt

	| caughtIt |
	caughtIt := false.
	[self halt] on: ControlInterrupt do: [:ex | caughtIt := true ].
	self assert: caughtIt.
	caughtIt := false.
	[System 
		signal: 6003 "#rtErrSoftBreak" 
		args: {#rtErrSoftBreak. }
		signalDictionary: GemStoneError] on: ControlInterrupt do: [:ex | caughtIt := true ].
	self assert: caughtIt.
