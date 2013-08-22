initializing
setMetaNode: aMetaNode node: aNode 
	root _ aNode.
	root metaNode: aMetaNode.
	self pushColumn: (OBColumn inPanel: self metaNode: aMetaNode node: root).
	minPanes - self columns size 
		timesRepeat: [self pushColumn: self emptyColumn]