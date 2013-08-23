updating
refreshAndSignal: aBoolean
	| node shouldSignal ans |
	shouldSignal := aBoolean.
	self isEmpty ifTrue: [^self]. 
	node := self selectedNode.
	ans := self listHolder refresh.
	ans ifTrue:
		[self selectSilently: node.
		self hasSelection 
			ifTrue: [ self refreshMultipleSelections ~~ nil ifTrue: [ shouldSignal := true ] ]
			ifFalse:  [shouldSignal := true]].
	shouldSignal ifTrue: [self signalSelectionChanged].
	self changed: #list