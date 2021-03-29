*bootstrap
testRunSuiteWithResource
	| suite x |
	suite := TestSuite new.
	suite addTest: (SimpleTestResourceTestCase selector: #error).
	suite addTest: (SimpleTestResourceTestCase selector: #fail).
	suite addTest: (self class selector: #dummy).
	self clearOuterResourceStateDuring:
		[self assert: (x := suite run printString) = '3 run, 1 passes, 0 expected defects, 1 failures, 1 errors, 0 unexpected passes'
			description: 'A suite of tests needing SimpleTestResource did not run as expected'].