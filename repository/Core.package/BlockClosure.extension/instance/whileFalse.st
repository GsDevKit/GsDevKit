*core-squeak
whileFalse
	"Evaluate the receiver once and then repeatedly as long as the value
	returned by the evaluation is true."

	^[self value] whileFalse: []