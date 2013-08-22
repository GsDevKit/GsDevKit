nodes
allChildrenForNode: aNode
	^ edges gather: [:edge | edge nodesForParent: aNode]