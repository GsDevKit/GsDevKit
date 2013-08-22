navigating
addChild: aNode
	(collection includes: aNode)
		ifFalse: [collection _ collection copyWith: aNode]