*gssqueakcommon-core
encodeForHTTPWithTextEncoding: encodingName 
	"change dangerous characters to their %XX form, for use in HTTP transactions"
	^ self encodeForHTTPWithTextEncoding: encodingName conditionBlock: [:c | c isSafeForHTTP].