filtering
nodesFrom: aCollection forNode: aNode
	^ nodes 
		ifNil: [super nodesFrom: aCollection forNode: aNode]
		ifNotNil: [nodes value: aCollection value: aNode]