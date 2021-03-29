*core-squeak
charCode
	"assuming Unicode"
	
	^(self asciiValue bitAnd: 16r3FFFFF)