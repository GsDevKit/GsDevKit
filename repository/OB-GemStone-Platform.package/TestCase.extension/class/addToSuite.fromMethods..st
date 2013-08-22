*ob-gemstone-platform-GSExtensions
addToSuite: suite fromMethods: testMethods 
	testMethods do:  [ :selector | 
			suite addTest: (self selector: selector) ].
	^suite