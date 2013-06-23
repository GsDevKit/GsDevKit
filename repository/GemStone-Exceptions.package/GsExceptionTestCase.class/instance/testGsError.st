tests
testGsError

	| caughtIt resumed |
	resumed := caughtIt := false.
	[System 
		signal: 1031 "#StDBErrNotDefined" 
		args: {#StDBErrNotDefined. }
		signalDictionary: GemStoneError.
	resumed := true] on: GsError do: [:ex | caughtIt := true. ex resume ].
	self assert: caughtIt.
	self assert: resumed.
	resumed := caughtIt := false.
	[System 
		signal: 2011 "#objErrNotSegment" 
		args: {#objErrNotSegment. }
		signalDictionary: GemStoneError.
	resumed := true] on: GsError do: [:ex | caughtIt := true. ex resume ].
	self assert: caughtIt.
	self assert: resumed.
	resumed := caughtIt := false.
	[System 
		signal: 3015 "#bkupErrDisallowed" 
		args: {#bkupErrDisallowed. }
		signalDictionary: GemStoneError.
	resumed := true] on: GsError do: [:ex | caughtIt := true. ex resume ].
	self assert: caughtIt.
	self assert: resumed.
