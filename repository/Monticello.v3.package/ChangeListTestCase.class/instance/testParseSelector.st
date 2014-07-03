tests
testParseSelector
	| x | 
	self assert: (x := ChangeList parseSelector: 'test') = 'test'.
	self assert: (x := ChangeList parseSelector: 'test2') = 'test2'.
	self assert: (x := ChangeList parseSelector: 'test: arg1') = 'test:'.
	self assert: (x := ChangeList parseSelector: 'test: arg1 test: arg2') = 'test:test:'.
	self assert: (x := ChangeList parseSelector: 'test
		x := 1
		^x') = 'test'.
	self assert: (x := ChangeList parseSelector: 'test: arg1
		| x |
		x := 1
		^x') = 'test:'.
	self assert: (x := ChangeList parseSelector: 'test: arg1 test: arg2
		<primitive: 617 >
		^nil') = 'test:test:'.
	self assert: (x := ChangeList parseSelector: 'test: arg1 test: arg2
		""test""
		<primitive: 617 >
		^nil') = 'test:test:'.
	self assert: (x := ChangeList parseSelector: 'debit: aCreditCard from: anAmount
        
') = 'debit:from:'.