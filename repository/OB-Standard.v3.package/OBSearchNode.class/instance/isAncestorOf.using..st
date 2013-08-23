ancestry
isAncestorOf: aNode using: aSelector
	^ self = aNode or: [self classSatisfiesQuery: aNode theClass]