accessing
addMetaNode: aMetaNode
	^ self metaNodes
		at: aMetaNode name
		put: aMetaNode