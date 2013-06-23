signalling
replyToSenderWithSignal: aSignalNumber message: aString

	System 
		_sendSignal: aSignalNumber 
		toSess: sessionSerialNum 
		withMessage: aString
