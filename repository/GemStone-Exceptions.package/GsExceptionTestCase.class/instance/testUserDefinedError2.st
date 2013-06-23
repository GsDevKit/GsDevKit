tests
testUserDefinedError2

	| caughtIt text |
	caughtIt := false.
	text := 'testing'.
	[System genericSignal: #genericError text: text] 
		on: UserDefinedError 
		do: [:ex | 
			caughtIt := true.
			self assert: ex messageText = text ].
	self assert: caughtIt.
