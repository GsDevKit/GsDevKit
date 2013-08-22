nodes
nodesForParent: aNode
	^ edges gather: [:edge | edge nodesForParent: aNode]