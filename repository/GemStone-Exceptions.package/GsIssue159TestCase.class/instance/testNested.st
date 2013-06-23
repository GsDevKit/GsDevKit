tests
testNested

	| caught exception args |
	caught := false.
	[ self resendIssue159 ] on: MessageNotUnderstood do: [:ex | 
		exception := ex.
		caught := true ]. 
	self assert: caughtIt.
	self assert: caught.

	self assert: exception class == MessageNotUnderstood.
	args := exception gsArguments.
	self assert: (args at: 1) == self.
	self assert: (args at: 2) == #nestedFooIssue159.
	self assert:  (args at: 3) size == 0