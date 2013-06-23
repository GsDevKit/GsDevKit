ANSI support
signal: signalText

	ExceptionA hasFixFor39741 ifFalse: [
		self shouldNotImplement: #signal:.
	].
	^super signal: signalText.
	