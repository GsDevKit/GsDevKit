*gssqueakcommon-core
readHexFrom: aString
	"Create a byte array from a hexadecimal representation"
	^(self new: aString size // 2) readHexFrom: aString readStream