private
primEncodePrimDecode: sample

	|  encoded decoded |
	encoded := sample encodeAsUTF8.
	self assert: encoded class = String.
	decoded := encoded decodeFromUTF8.
	self assert: decoded = sample.
	^decoded
