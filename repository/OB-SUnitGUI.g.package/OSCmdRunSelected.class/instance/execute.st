execution
execute

	self clearTestCases.
	self testSuite == nil ifTrue: [ ^self ].
	self reset.
	self runTestSuite.
	self saveResultInHistory.