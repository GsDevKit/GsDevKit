selecting
selectAll

	selection := self children size.
	1 to: selection do: [:index |
		self multipleSelect: index put: true].
	self listHolder selection: selection .
     self signalSelectionChanged.
	self changed: #selection. 
	switch currentNode: self selectedNode.
	self changed: #multipleSelection