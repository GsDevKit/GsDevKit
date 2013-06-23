private
primEncodeDecode: sample

	|  encoded decoded |
	encoded := sample encodeAsUTF8.
	self assert: encoded class = String.
	decoded := UTF8Encoding decode: encoded.
	self assert: decoded = sample.
	^decoded
