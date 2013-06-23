ANSI - exceptionSignaler
_isHandlerFor: anInteger dict: aSymbolDictionary exceptionA: anExceptionA args: anArray

	^anInteger = self errorNumberToSignal and: [
		aSymbolDictionary = self exceptionCategory and: [
		anExceptionA isNil and: [
		anArray = self exceptionArguments]]].
