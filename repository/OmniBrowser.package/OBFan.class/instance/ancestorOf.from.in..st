selecting
ancestorOf: aNode from: aCollection in: aBlock
	aCollection doWithIndex:
		[:node :index | 
		(node isAncestorOf: aNode using: node metaNode ancestrySelector)
			ifTrue: [aBlock value: index. ^ node]].
	^ nil