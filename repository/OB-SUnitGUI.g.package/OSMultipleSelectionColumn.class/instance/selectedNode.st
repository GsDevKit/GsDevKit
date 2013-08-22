selecting
selectedNode
	
	| node |
	node := listHolder selectedNode.
	(node ~~ nil and: [node acceptsCoParents ]) ifTrue: [ node coParents: self selectedNodes ].
	^node