*core-squeak
digitLength
	"Returns the number of base 256 digits in the receiver, without counting
	 any leading zeros."

	| abs n |
	abs := self abs.
	n := 1.
	[	abs < (1 bitShift: n*8) ifTrue: [ ^n ].
		n := n +1.
		true ] whileTrue