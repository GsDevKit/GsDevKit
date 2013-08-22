execution
execute

	| suite testCases |
	self clearTestCases.
	testCases := self result failures.
	suite := TestSuite new.
	suite 
		addTests: testCases; 
		name: (self label: 'Failure' forSuite: suite).
	self result instVarNamed: 'failures' put: OrderedCollection new.
	self runSuite: suite.