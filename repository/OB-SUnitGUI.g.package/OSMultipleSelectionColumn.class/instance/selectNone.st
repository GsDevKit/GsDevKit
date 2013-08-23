selecting
selectNone

	selection := 0.
	1 to: self children size do: [:index |
		self multipleSelect: index put: false].
	self listHolder selection: selection .
     self signalSelectionChanged.
	self changed: #selection. 
	switch currentNode: self selectedNode.
	self changed: #multipleSelection