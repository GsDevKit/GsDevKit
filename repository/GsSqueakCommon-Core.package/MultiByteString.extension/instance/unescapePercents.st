*gssqueakcommon-core
unescapePercents
	"change each %XY substring to the character with ASCII value XY in hex.  This is the opposite of #encodeForHTTP.
	 Assume UTF8 encoding for Pharo compatibility"

	"this method not implemented in CharacterCollection in order to preserve original behavior without overrides"
	^self unescapePercentsWithTextEncoding: 'utf-8'