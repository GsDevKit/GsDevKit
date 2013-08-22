navigating
removeChild: aNode
	(collection includes: aNode)
		ifTrue: [collection _ collection copyWithout: aNode]