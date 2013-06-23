ANSI - exceptionSignaler
signal: signalText

	^ExceptionA hasFixFor39741 ifTrue: [
		super signal: signalText.
	] ifFalse: [
		self 
			signalError: self errorNumberToSignal 
			args: (self exceptionArguments: signalText).
	].
