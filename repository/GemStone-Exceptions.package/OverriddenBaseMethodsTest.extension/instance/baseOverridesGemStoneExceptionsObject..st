*gemstone-exceptions
baseOverridesGemStoneExceptionsObject: known
	self
		addTo: known
		forBehavior: Object
		overrides: {
			#'pause' -> 'pause

"Generates an error.  You can use this method to establish
 breakpoints in methods, aside from any debugger breakpoints that
 may be set."

<primitive: 906>
^ self  "continue with execution"
'.
			#'cantPerform:withArguments:' -> 'cantPerform: aSelectorSymbol withArguments: anArray

"This method implements the default response when a message can''t be performed
 with _perform:withArguments:.  It raises the rtErrCantPerform exception."

self _error: #rtErrCantPerform args: #[aSelectorSymbol, anArray].
^ self _primitiveFailed: #perform:withArguments:
'.
			#'halt:' -> 'halt: messageString

"Raises an error.  This method is intended for use in raising
 application-defined or user-defined errors. Returns the receiver."

System genericSignal: #halt text: messageString.
^ self
'.
			#'error:' -> 'error: messageString

"Raises an error.  This method is intended for use in raising
 application-defined or user-defined errors. Returns the receiver."

^ self halt: messageString.
'.
	}