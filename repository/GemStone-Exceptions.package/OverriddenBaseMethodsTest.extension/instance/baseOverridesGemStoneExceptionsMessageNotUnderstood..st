*gemstone-exceptions
baseOverridesGemStoneExceptionsMessageNotUnderstood: known
	self
		addTo: known
		forBehavior: MessageNotUnderstood
		overrides: {
			#'signal' -> 'signal

	nil fooBar.	"We hope this message is not understood!"
'.
	}