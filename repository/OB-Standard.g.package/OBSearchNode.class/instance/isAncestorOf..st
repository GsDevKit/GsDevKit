ancestry
isAncestorOf: aNode
	^ self = aNode or: [self classSatisfiesQuery: aNode theClass]