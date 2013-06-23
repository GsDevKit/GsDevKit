private
primEncodePrimDecodeDBS: sample

	|  encoded decoded |
	encoded := sample encodeAsUTF8.
	self assert: encoded class = String.
	decoded := encoded decodeIntoDoubleByteStringFromUTF8.
	self assert: decoded = sample.
	^decoded
