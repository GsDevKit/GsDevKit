ANSI - exceptionSignaler
signal
    "use class side protocal"

	^ExceptionA hasFixFor39741 ifTrue: [
		super signal.
	] ifFalse: [
		self shouldNotImplement: #signal.
	].
