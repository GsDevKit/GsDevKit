tests
testIssue159

	caughtIt := false.
	self fooIssue159. 
	self assert: caughtIt.
	caughtIt := false.
	self perform: #fooIssue159.
	self assert: caughtIt.
