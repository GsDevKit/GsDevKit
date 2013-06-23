*GemStone-Exceptions
error: messageString

"Raises an error.  This method is intended for use in raising
 application-defined or user-defined errors. Returns the receiver."

^ExceptionA hasFixFor39741 ifTrue: [
	UserDefinedError signal: messageString.
] ifFalse: [
	self halt: messageString.
].
