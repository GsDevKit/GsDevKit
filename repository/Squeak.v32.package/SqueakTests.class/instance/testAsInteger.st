testing
testAsInteger

	self assert: '123' asInteger = 123.
	self assert: '12r' asInteger = 12.
	self assert: ('12r', (String with: (Character value: 8364))) asInteger = 12.
	self assert: 'error' asInteger == nil