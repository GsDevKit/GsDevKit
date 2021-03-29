tests
testAssertDescription

	[nil assert: [ false ] description: 'hi there'] 
	on: AssertionFailure 
	do: [:ex | self assert: ex messageText = 'hi there' ]