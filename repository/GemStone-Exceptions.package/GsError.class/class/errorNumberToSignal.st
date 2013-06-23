ANSI support
errorNumberToSignal

	ExceptionA hasFixFor39741 ifTrue: [
		^1.
	] ifFalse: [
		self subclassResponsibility: #errorNumberToSignal.
	].
