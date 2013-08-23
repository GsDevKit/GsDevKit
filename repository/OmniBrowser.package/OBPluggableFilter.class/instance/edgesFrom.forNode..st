filtering
edgesFrom: aCollection forNode: aNode
	^ edges 
		ifNil: [super edgesFrom: aCollection forNode: aNode]
		ifNotNil: [edges value: aCollection value: aNode]