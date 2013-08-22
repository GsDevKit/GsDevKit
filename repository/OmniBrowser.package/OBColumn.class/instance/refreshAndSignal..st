updating
refreshAndSignal: aBoolean
	| node shouldSignal |
	shouldSignal := aBoolean.
	self isEmpty ifTrue: [^self]. 
	node := self selectedNode.
	self listHolder refresh ifTrue:
		[self selectSilently: node.
		self hasSelection ifFalse: 
			[shouldSignal := true]].
	shouldSignal ifTrue: [self signalSelectionChanged].
	self changed: #list