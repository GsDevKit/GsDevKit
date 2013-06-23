tests
testUserDefinedError5

	| caughtIt text  |
	caughtIt := false.
	text := 'testing'.
	[System 
		signal: UserDefinedError errorNumberToSignal 
		args: {#genericError. text. #(). }
		signalDictionary: GemStoneError] 
			on: UserDefinedError 
			do: [:ex | 
				caughtIt := true.
				self assert: ex messageText = text ].
	self assert: caughtIt.
