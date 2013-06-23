*gemstone-exceptions
baseOverridesGemStoneExceptionsMessageNotUnderstoodClass: known
	self
		addTo: known
		forBehavior: MessageNotUnderstood class
		overrides: {
			"2.4.x"
			#'signal:' -> 'signal: anObject

	nil fooBar.	"We hope this message is not understood!"
'.
			"2.3.x"
			#'signal:' -> 'signal: anObject

	anObject fooBar.
'.
	}