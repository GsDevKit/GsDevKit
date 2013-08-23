menu messages
printIt: aString in: aContext

	^(self evaluate: aString inContext: aContext) printString