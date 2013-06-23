testing
testDoubleByteLargeStringDecode
	"Verify that for double byte strings, the primDecoded string is of class Quad/DoubleByteString"
	
	| sample decoded |
	sample := self generateRandomDoubleByteString: 20000.
	self assert: sample _isLarge.
	self assert: sample class == DoubleByteString.
	decoded := self primEncodePrimDecode: sample.
	self in2x
		ifTrue: [ self assert: decoded class == QuadByteString ]
		ifFalse: [ self assert: decoded class == Unicode16 ].
	decoded := self primEncodePrimDecodeDBS: sample.
	self assert: decoded class == Unicode16.
