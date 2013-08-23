children
childsPerEdgeFor: aNode 
	"collects all childs and returns an array with one list of childs per edge.
	all filters are applied to the child lists."
	^ edges
		collect: [:edge | filters
							inject: (edge nodesForParent: aNode)
							into: [:list :filter | filter nodesFrom: list forNode: aNode]].