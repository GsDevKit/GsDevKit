*core-squeak
repeat
	"Evaluate the receiver repeatedly, ending only if the block
	forces some stopping condition."

	[true] whileTrue: [self value]