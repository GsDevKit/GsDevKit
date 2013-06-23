tests
testAbortingError

	| caughtIt |
	caughtIt := false.
	[System 
		signal: 3015 "#bkupErrDisallowed" 
		args: {#bkupErrDisallowed. }
		signalDictionary: GemStoneError] on: AbortingError do: [:ex | caughtIt := true ].
	self assert: caughtIt.
