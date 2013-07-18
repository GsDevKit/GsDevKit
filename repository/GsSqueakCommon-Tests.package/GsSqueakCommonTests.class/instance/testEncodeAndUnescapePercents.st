testing
testEncodeAndUnescapePercents

	| str encoded encoded_UTF8 encoded_LATIN1|
	str := String new.
	{ 16rFF. 16rA. } do: [:byte | str add: byte asCharacter ].
	encoded_UTF8 := str copy encodeForHTTPWithTextEncoding: 'utf-8'.
	self assert: (encoded_UTF8 unescapePercentsWithTextEncoding: 'utf-8') = str.
	encoded := str copy encodeForHTTP.
	self assert: encoded unescapePercents  = str.
	encoded := str copy encodeForHTTP.
	self assert: (encoded unescapePercentsWithTextEncoding: 'utf-8')  = str.
	encoded := str copy encodeForHTTPWithTextEncoding: 'utf-8'.
	self assert: encoded unescapePercents  = str.
	encoded_LATIN1 := str copy encodeForHTTPWithTextEncoding: 'latin-1'.
	self assert: (encoded_LATIN1 unescapePercentsWithTextEncoding: 'latin-1')  = str.
