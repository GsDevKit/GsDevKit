filtering
displayString: aString forParent: pNode child: cNode
	^ parentDisplay
		ifNil: [super displayString: aString forParent: pNode child: cNode]
		ifNotNil: [parentDisplay value: aString value: pNode value: cNode]