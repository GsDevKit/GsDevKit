other
signal

	ExceptionA hasFixFor39741 ifFalse: [
		self shouldNotImplement: #signal.
	].
	^super signal.
	