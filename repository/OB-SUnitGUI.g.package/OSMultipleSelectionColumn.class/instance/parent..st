accessing
parent: aNode 
	
	| cNode |
	self basicParent: aNode.
	(cNode := self refreshMultipleSelections) == nil
		ifTrue: [ cNode := self listHolder autoSelection ].
	self jumpTo: cNode