ancestry
isAncestorOf: aNode 
	^ (aNode source findString: selector string startingAt: 0 ) isNonZero.