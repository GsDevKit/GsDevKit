accessing
parent: aNode 
	| cNode |
	self basicParent: aNode.
	cNode := self listHolder autoSelection.
	self jumpTo: cNode