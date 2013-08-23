initialization
setMetaNode: aMetaNode node: aNode 
	super setMetaNode: aMetaNode node: aNode.
	theClass := aNode theClass.
	filter := OBLimitProtocolFilter on: aNode theClass.
	aMetaNode addFilter: filter