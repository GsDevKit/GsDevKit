testing
testRandomString
	
	| sample |
	sample := self generateRandomString: 2000.
	self deny: sample _isLarge.
	self assert: sample class == QuadByteString.
	self encodeDecode: sample.
	self encodePrimDecode: sample.
	self primEncodeDecode: sample.
	self primEncodePrimDecode: sample.
	self encodePrimDecodeDBS: sample.
	self primEncodePrimDecodeDBS: sample.
