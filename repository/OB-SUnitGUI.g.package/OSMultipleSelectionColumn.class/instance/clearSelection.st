selecting
clearSelection

	self multipleSelection keys do: [:key | self multipleSelect: key put: false ].
	selection := nil.
	super clearSelection.
	self changed: #multipleSelection.
	self changed: #selection.
     self signalSelectionChanged.