filtering
displayStringForNode: aNode
	^ filters
		inject: (aNode perform: self displaySelector)
		into: [:string :filter | filter displayString: string forNode: aNode]