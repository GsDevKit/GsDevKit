*gssqueakcommon-core
encodeForHTTP
	"change dangerous characters to their %XX form, for use in HTTP transactions.
	 Assume UTF8 encoding for Pharo compatibility"

	"this method not implemented in CharacterCollection in order to preserve original behavior without overrides"
	
	^self encodeForHTTPWithTextEncoding: 'utf-8'