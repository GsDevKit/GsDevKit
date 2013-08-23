accessing
selector

	label == nil ifTrue: [^'x'].
	^label subStrings last asSymbol.