*core-squeak
digitAt: n
	"Answer the nth base 256 digit in the receiver."

	self < 0
		ifTrue: [ ^((0-self) bitShift: (1-n)*8) bitAnd: 16rFF ]
		ifFalse: [ ^(self bitShift: (1-n)*8) bitAnd: 16rFF ]