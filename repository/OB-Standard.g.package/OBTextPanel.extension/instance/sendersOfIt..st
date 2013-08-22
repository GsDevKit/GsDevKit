*ob-standard-cmds
sendersOfIt: aSelector 
	| node |
	node := OBSelectorNode on: aSelector.
	(self announce: (OBShowSenders of: node)) execute.
	^true