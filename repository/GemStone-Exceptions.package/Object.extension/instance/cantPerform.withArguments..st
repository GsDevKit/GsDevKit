*GemStone-Exceptions
cantPerform: aSelectorSymbol withArguments: anArray
	"Fix for Issue 159"

	^self doesNotUnderstand: { aSelectorSymbol. anArray. }
