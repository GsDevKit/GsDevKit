tests
testUserDefinedError3

	| caughtIt text |
	caughtIt := false.
	text := 'testing'.
	[self error: text] 
		on: UserDefinedError 
		do: [:ex | 
			caughtIt := true.
			self assert: ex messageText = text ].
	self assert: caughtIt.
