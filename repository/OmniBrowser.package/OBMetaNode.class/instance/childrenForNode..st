nodes
childrenForNode: aNode
	| chosenEdges | 
	chosenEdges := self edgesForNode: aNode.
	^ chosenEdges gather: 
		[:edge | 
		filters 
			inject: (edge nodesForParent: aNode)
			into: [:list :filter | filter nodesFrom: list forNode: aNode]].