utility
referenceForMethod: selector ofClass: className
	| classIsMeta symbol |
	classIsMeta _ className endsWith: ' class'.
	symbol _ classIsMeta ifTrue: [(className allButLast: 6) asSymbol] ifFalse: [className].
	^ (MethodReference new 
		setClassSymbol: symbol
		classIsMeta: classIsMeta
		methodSymbol: selector
		stringVersion: symbol, '>>', selector)
			symbolDictionary: self symbolDictionary;
			yourself