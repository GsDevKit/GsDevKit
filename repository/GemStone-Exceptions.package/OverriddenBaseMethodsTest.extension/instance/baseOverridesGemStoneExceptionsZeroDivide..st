*gemstone-exceptions
baseOverridesGemStoneExceptionsZeroDivide: known
	self
		addTo: known
		forBehavior: ZeroDivide class
		overrides: {
			"2.3.x only"
			#'signal' -> 'signal

	0 / 0.
'.
	}