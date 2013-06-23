accessing
contentString
	^(self contents size == 1
		and: [self contents first isKindOf: XMLStringNode])
		ifTrue: [self contents first string]
		ifFalse: ['']