execution
execute

	| suite testCases |
	self clearTestCases.
	testCases := self result errors.
	suite := TestSuite new.
	suite 
		addTests: testCases; 
		name: (self label: 'Error' forSuite: suite).
	self result instVarNamed: 'errors' put: OrderedCollection new.
	self runSuite: suite.