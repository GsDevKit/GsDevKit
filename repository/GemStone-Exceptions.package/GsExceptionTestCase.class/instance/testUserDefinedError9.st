tests
testUserDefinedError9

	| caughtIt text list |
	caughtIt := false.
	text := 'testing'.
	[UserDefinedError signal: text args: {#genericError. nil. #(#arg). }] 
			on: UserDefinedError 
			do: [:ex | 
				caughtIt := true.
				self 
					assert: ex messageText = text;
					assert: (list := ex gsArguments) = #(#genericError 'testing' #(#arg));
					yourself.
			].
	self assert: caughtIt.
