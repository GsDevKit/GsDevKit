*gemstone-exceptions
isHandlerFor: anInteger dict: aSymbolDictionary exceptionA: anExceptionA args: anArray

	^self == ExceptionA or: [anExceptionA isKindOf: self].
