tests
testAssert

	self 
		should: [ nil assert: [ false ] ]
		raise: AssertionFailure