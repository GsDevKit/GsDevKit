private
checkEncoding: codePoint using: encoder

	| a b |
	a := encoder encode: codePoint asCharacter asString.
	b := codePoint asCharacter asString encodeAsUTF8.
	self assert: a = b.
