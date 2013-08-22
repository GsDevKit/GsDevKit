*ob-standard-cmds
implementorsOfIt: aSelector 
	| node |
	node := OBSelectorNode on: aSelector.
	(self announce: (OBShowImplementors of: node)) execute.
	^true