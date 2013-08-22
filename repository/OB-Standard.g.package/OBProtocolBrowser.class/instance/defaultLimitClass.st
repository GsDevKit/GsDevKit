accessing
defaultLimitClass
	^ (theClass isKindOf: Metaclass) 
		ifTrue: [Object class]
		ifFalse: [Object]