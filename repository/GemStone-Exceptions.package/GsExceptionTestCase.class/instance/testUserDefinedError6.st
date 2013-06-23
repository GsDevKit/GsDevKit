tests
testUserDefinedError6

	| caughtIt text list |
	caughtIt := false.
	text := 'testing'.
	[UserDefinedError signal] 
			on: UserDefinedError 
			do: [:ex | 
				| string |
				caughtIt := true.
				self 
					assert: ex messageText == nil;
					assert: ex tag == nil;
					assert: (list := ex gsArguments) = #(#'genericError' nil #());
					assert: (string := ex description) = 'UserDefinedError 2318: User defined error, <nil>';
					yourself.
			].
	self assert: caughtIt.
