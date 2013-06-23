private
encodeString: aString 
	"from Squeak 4.x SMTPClient>>#encodeString:
	credited: 'ar 12/4/2009 10:08 · utility' "
	| str dec out |
	str := String new: (aString size * 4 / 3 + 3) ceiling.
	dec := Base64MimeConverter new.
	dec
		mimeStream: (out := WriteStream on: str);
		dataStream: (ReadStream on: aString);
		"multiLine: false;"
		mimeEncode.
	^out contents