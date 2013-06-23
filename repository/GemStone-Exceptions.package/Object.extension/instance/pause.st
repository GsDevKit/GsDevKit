*GemStone-Exceptions
pause

"Generates an error.  You can use this method to establish
 breakpoints in methods, aside from any debugger breakpoints that
 may be set."

"<primitive: 906>"
ExceptionA hasFixFor39741 ifTrue: [
	Pause signal.
] ifFalse: [
	System 
		signal: 6001 
		args: #() 
		signalDictionary: GemStoneError.
].
^ self  "continue with execution"
