filtering
displayStringForParent: pNode child: cNode
	^ filters
		inject: cNode displayString
		into: [:string :filter | filter displayString: string forParent: pNode child: cNode]