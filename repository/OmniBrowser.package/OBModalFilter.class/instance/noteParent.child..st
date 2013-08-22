filtering
noteParent: pNode child: cNode 
	pNode metaNode defaultEdges keysAndValuesDo: 
		[ :index :edge | 
		cNode metaNode = edge metaNode ifTrue: [ selection := index ] ]