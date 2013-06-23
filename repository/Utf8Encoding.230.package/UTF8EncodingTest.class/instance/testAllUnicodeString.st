testing
testAllUnicodeString
	"Generate a large QuadByteString"
	
	| sample  |
	sample := self generateAllGsUnicodeString.
	self assert: sample _isLarge.
	self assert: sample class == QuadByteString.
	self encodeDecode: sample.
	self encodePrimDecode: sample.
	self primEncodeDecode: sample.
	self primEncodePrimDecode: sample.
	self encodePrimDecodeDBS: sample.
	self primEncodePrimDecodeDBS: sample.
