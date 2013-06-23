name space
qualifiedName
	^self namespace
		ifNil: [self localName]
		ifNotNil: [self namespace , ':' , self localName]