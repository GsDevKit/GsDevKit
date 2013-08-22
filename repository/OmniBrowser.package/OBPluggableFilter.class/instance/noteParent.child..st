filtering
noteParent: pNode child: cNode
	^ note 
		ifNil: [super noteParent: pNode child: cNode]
		ifNotNil: [note value: pNode value: cNode]