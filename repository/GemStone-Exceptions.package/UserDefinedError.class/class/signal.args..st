ANSI - exceptionSignaler
signal: signalText args: anArray

	anArray at: 2 put: signalText.
	^ExceptionA hasFixFor39741 ifTrue: [
		self new
			gsArguments: anArray;
			signal: signalText.
	] ifFalse: [
		self 
			signalError: self errorNumberToSignal 
			args: anArray.
	].
