tests
testPauseAndError

	| caught |
	[
		[
			self pause.
		] on: Error do: [:ex | 
			caught := Error.
		].
	] on: Halt do: [:ex | 
		caught := Halt.
	].
	self assert: caught == Halt.
