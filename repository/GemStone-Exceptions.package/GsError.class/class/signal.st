ANSI - exceptionSignaler
signal

    ^ExceptionA hasFixFor39741 ifTrue: [
		super signal.
	] ifFalse: [
		System 
			signal: self errorNumberToSignal 
			args: self exceptionArguments 
			signalDictionary: self exceptionCategory.
	].
