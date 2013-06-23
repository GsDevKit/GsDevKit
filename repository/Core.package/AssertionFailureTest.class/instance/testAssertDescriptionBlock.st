tests
testAssertDescriptionBlock

	[nil assert: [ false ] descriptionBlock: [ 'hi there' ]] 
	on: AssertionFailure 
	do: [:ex | self assert: (ex messageText endsWith: 'hi there') ]