ANSI support
signal

	ExceptionA hasFixFor39741 ifFalse: [
	   ^self 
			signalError: self errorNumberToSignal 
			args: self exceptionArguments 
			signalDictionary: self exceptionCategory.
	].
	^super signal.
	