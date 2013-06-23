tests
testHaltAndError

	| caughtIt |
	[[self halt] 
		on: Error 
		do: [:ex | caughtIt := true ]
	] on: Halt do: [:ex | caughtIt := false. ].
	self deny: caughtIt