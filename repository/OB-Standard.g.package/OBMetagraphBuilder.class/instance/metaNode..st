accessing
metaNode: aMetaNodeName 
	^ self metaNodes
		at: aMetaNodeName
		ifAbsentPut:[OBMetaNode named: aMetaNodeName]