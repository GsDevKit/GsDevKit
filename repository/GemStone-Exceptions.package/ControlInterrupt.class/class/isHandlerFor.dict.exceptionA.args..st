ANSI - exceptionSignaler
isHandlerFor: anInteger dict: aSymbolDictionary exceptionA: anExceptionA args: anArray

	(super isHandlerFor: anInteger dict: aSymbolDictionary exceptionA: anExceptionA args: anArray) ifTrue: [^true].
	self withAllSubclasses do: [:each | 
		(each
			_isHandlerFor: anInteger 
			dict: aSymbolDictionary 
			exceptionA: anExceptionA 
			args: anArray) ifTrue: [^true].
	].
	^anInteger // 1000 = 6 and: [aSymbolDictionary = GemStoneError].
