tests
testInvalid
	self assert: (self invalidFixtures noneSatisfy: [ : each | each isValid ])