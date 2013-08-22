selecting
selectInversion

	selection := 0.
	1 to: self children size do: [:index |
		(self multipleSelection at: index ifAbsent: [ false ])
			ifTrue: [ self multipleSelect: index put: false ]
			ifFalse: [
				self multipleSelect: index put: true.
				selection := index]].
	self listHolder selection: selection .
     self signalSelectionChanged.
	self changed: #selection. 
	switch currentNode: self selectedNode.
	self changed: #multipleSelection