tests
testUserDefinedError1

	| exA list |
	exA := Error 
		category: GemStoneError
		number: 2318
		arguments: #(#'genericError' nil #()).
	self
		assert: exA class = UserDefinedError;
		assert: (list := exA gsArguments) = #(#'genericError' nil #());
		yourself.
