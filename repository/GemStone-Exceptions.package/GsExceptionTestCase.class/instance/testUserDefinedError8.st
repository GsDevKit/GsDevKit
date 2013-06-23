tests
testUserDefinedError8

	| caughtIt text |
	caughtIt := false.
	text := 'testing'.
	[UserDefinedError signal: nil args: {#genericError. nil. #(#arg). }] 
			on: UserDefinedError 
			do: [:ex | 
				caughtIt := true.
				self assert: ex messageText == nil.
				self assert: ex description = 'UserDefinedError 2318: User defined error, <nil>' ].
	self assert: caughtIt.
