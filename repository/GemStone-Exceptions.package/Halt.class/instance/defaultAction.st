other
defaultAction

	^System 
		_signalGciError: 	self class errorNumberToSignal
		args: 					self gsArguments
		signalDictionary: 	self class exceptionCategory.
