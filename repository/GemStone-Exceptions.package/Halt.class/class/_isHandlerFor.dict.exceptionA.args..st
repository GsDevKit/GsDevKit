ANSI support
_isHandlerFor: anInteger dict: aSymbolDictionary exceptionA: anExceptionA args: anArray

	(super _isHandlerFor: anInteger dict: aSymbolDictionary exceptionA: anExceptionA args: anArray) ifTrue: [^true].
	^anInteger = self errorNumberForPause and: [
		aSymbolDictionary = self exceptionCategory].
