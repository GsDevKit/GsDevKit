tests
testUserDefinedError7

	| caughtIt text |
	caughtIt := false.
	text := 'testing'.
	[UserDefinedError signal: text] 
			on: UserDefinedError 
			do: [:ex | 
				caughtIt := true.
				self assert: ex messageText = text ].
	self assert: caughtIt.
