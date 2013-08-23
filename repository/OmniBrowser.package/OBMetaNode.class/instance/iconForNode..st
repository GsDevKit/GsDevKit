filtering
iconForNode: aNode
	^ filters inject: nil into: [:icon :filter | filter icon: icon forNode: aNode]