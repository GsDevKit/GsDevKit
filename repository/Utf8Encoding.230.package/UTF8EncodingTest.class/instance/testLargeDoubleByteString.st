testing
testLargeDoubleByteString
	
	| sample decoded |
	sample := self generateRandomDoubleByteString: 20000.
	self assert: sample _isLarge.
	self assert: sample class == DoubleByteString.
	self encodeDecode: sample.
	self encodePrimDecode: sample.
	self encodePrimDecodeDBS: sample.
	self primEncodeDecode: sample.
	decoded := self primEncodePrimDecodeDBS: sample.
	self assert: decoded class == DoubleByteString.
	decoded := self primEncodePrimDecode: sample.
	self in2x
		ifTrue: [ self assert: decoded class == QuadByteString ]
		ifFalse: [ self assert: decoded class == DoubleByteString ].
