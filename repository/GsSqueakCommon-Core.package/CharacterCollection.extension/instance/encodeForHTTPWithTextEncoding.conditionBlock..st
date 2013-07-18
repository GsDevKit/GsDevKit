*gssqueakcommon-core
encodeForHTTPWithTextEncoding: encodingName conditionBlock: conditionBlock
	"change dangerous characters to their %XX form, for use in HTTP transactions"
	| encodedStream |
	encodedStream := WriteStream on: (String new).
	
	((GRCodec forEncoding: encodingName) encode: self) asString do: [ :c | 
		(conditionBlock value: c) ifTrue: [ encodedStream nextPut: c ] ifFalse: [
			encodedStream nextPut: $%.
			encodedStream nextPut: (c asciiValue // 16) asHexDigit asUppercase.
			encodedStream nextPut: (c asciiValue \\ 16) asHexDigit asUppercase.
		]
	].
	^encodedStream contents.