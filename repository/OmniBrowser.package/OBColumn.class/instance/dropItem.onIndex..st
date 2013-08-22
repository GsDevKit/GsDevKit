callbacks
dropItem: passenger onIndex: item
	| aListItem theNode dropSelector oldIndex |
	OBPlatform current doAutoAbort.
	oldIndex := self selection.
	aListItem := self selection: item.
	theNode := self selectedNode.
	(theNode respondsTo: #dropSelector) 
		ifTrue: [
		"wantsDroppedNode: is not used at the moment"
		dropSelector := theNode dropSelector.
		(passenger respondsTo: dropSelector) 
			ifTrue: [passenger perform: dropSelector with: theNode] ].
	self selection: oldIndex.
	OBPlatform current doAutoCommit
		ifFalse: [OBInformRequest message: 'Auto commit failed. You will need to abort and try again.'].