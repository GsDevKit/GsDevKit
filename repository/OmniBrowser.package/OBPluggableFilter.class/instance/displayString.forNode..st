filtering
displayString: aString forNode: aNode
	^ nodeDisplay 
		ifNil: [super displayString: aString forNode: aNode]
		ifNotNil: [nodeDisplay value: aString value: aNode]