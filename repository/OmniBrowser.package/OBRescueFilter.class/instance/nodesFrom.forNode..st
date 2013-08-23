filtering
nodesFrom: aCollection forNode: aNode
	^ aCollection, (self cache: aCollection for: aNode)