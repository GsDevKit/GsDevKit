testing
testAsHexString


	self assert: 4 asHexString = '04'.
	self assert: 10 asHexString = '0a'.
	self assert: 15 asHexString = '0f'.
	self assert: 32 asHexString = '20'.