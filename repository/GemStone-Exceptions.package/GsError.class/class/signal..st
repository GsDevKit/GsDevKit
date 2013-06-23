ANSI - exceptionSignaler
signal: signalText
	"should probably use UserDefinedError if you are signalling a generic GsError and wish to pass in <signalText>"
	
	^ExceptionA hasFixFor39741 ifTrue: [
		super signal: signalText.
	] ifFalse: [
		self shouldNotImplement: #signal:.
	].
