tests
testHalt

	| caughtIt |
	caughtIt := false.
	[
		self halt.
	] on: Halt do: [:ex | 
		caughtIt := true.
		self assert: ex messageText = ''.
	].
	self assert: caughtIt.
