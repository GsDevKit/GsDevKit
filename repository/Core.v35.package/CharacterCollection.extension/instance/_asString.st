*core-squeak
_asString
	"Compatability method to avoid version specific branch to support QuadByteStrings for 2.3"
	
	^nil "character size can be greater than a single byte"