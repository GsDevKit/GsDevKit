*gemstone-exceptions
isHandlerFor: anInteger dict: aSymbolDictionary exceptionA: anExceptionA args: anArray

	| ansi |
	ansi := System myUserProfile objectNamed: #'ANSIException'.
	(ansi notNil and: [aSymbolDictionary = ansi]) ifTrue: [
		^anExceptionA isKindOf: self.
	].
	^(ControlInterrupt
		isHandlerFor: anInteger 
		dict: aSymbolDictionary 
		exceptionA: anExceptionA 
		args: anArray) not.
