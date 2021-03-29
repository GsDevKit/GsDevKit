initializing
setMetaNode: aMetaNode node: aNode 
	super setMetaNode: aMetaNode node: aNode.
	self navigationPanel selectSubtree: (OBSubtree new
		instVarAt: 1 put: (Array 
			with: aNode asFan
			with: 1);
		yourself)