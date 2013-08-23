accessing
testSuite

	| column testCase suite |
	column := requestor browser navigationPanel columns at: 2.
	testCase := column selectedNode.
	(testCase == nil or: [ testCase isAbstractTestCase ]) ifTrue: [ ^nil ].
	suite := testCase testSuite.
	suite name: (self label: 'Test' forSuite: suite).
	^suite