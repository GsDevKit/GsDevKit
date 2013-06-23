*gemstone-exceptions
handles: anExceptionSelector

	"This message determines whether the 'exception handler' associted with
	the receiver may be used to process the argument. Answer 'true' if an 
	associated handler should be used to process 'exception.' Answer 'false'
	if an associated handler may not be used to process the exception.

	Returns 'true' if the class of 'exception' is the receiver or a general
	subclass of the receiver.

	This definition implies that subclasses of an exception class are considered 
	to be 'subexceptions' of the type of exception defined by their superclass.
	An 'exception handler' that 'handles' an exception class will also handle any
	exceptions that are instances of the exception class's subclasses."

	^(anExceptionSelector isKindOf: self) or: [
		(anExceptionSelector isKindOf: ExceptionA) and: [anExceptionSelector gsNumber == self errorNumberToCatch].
	].
