tests
testPause

	| caughtIt |
	caughtIt := false.
	[self pause] on: Halt do: [:ex | caughtIt := true ].
	self assert: caughtIt