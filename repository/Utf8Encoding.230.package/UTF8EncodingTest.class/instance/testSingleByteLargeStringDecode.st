testing
testSingleByteLargeStringDecode
	"Verify that for single byte strings, the primDecoded string is of class String"
	
	| sample decoded |
	sample := self generateRandomSingleByteString: 20000.
	self assert: sample _isLarge.
	self assert: sample class == String.
	decoded := self primEncodePrimDecode: sample.
	self assert: decoded class == String.
	decoded := self primEncodePrimDecodeDBS: sample.
	self assert: decoded class == String.
