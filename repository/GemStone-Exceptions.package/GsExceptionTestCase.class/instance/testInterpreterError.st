tests
testInterpreterError

	| caughtIt text |
	caughtIt := false.
	text := 'testString'.
	[System 
		signal: 2011 "#objErrNotSegment" 
		args: {#objErrNotSegment. }
		signalDictionary: GemStoneError] on: InterpreterError do: [:ex | caughtIt := true ].
	self assert: caughtIt.
	caughtIt := false.
	[self error: text] 
		on: InterpreterError 
		do: [:ex | 
			caughtIt := true.
			self assert: ex messageText = text ].
	self assert: caughtIt.
	caughtIt := false.
	[UserDefinedError signal: text] 
			on: UserDefinedError 
			do: [:ex | 
				caughtIt := true.
				self assert: ex messageText = text ].
	self assert: caughtIt.
