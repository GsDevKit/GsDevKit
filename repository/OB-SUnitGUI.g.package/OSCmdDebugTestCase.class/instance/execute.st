execution
execute

	| suite |
	suite := TestSuite new.
	suite addTest: target testCase.
	self debugSuite: suite.