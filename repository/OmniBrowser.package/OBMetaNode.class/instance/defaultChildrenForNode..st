nodes
defaultChildrenForNode: aNode 
	| chosenEdges |
	chosenEdges := self defaultEdgesForNode: aNode.
	^ chosenEdges gather: 
		[:edge | 
		filters 
			inject: (edge nodesForParent: aNode)
			into: [:list :filter | filter nodesFrom: list forNode: aNode]].