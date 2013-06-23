*GemStone-Exceptions
halt: messageString

"Raises an error.  This method is intended for use in raising
 application-defined or user-defined errors. Returns the receiver."

ExceptionA hasFixFor39741 ifTrue: [
	Halt signal: messageString.
] ifFalse: [
	System genericSignal: #halt text: messageString.
].
^ self
