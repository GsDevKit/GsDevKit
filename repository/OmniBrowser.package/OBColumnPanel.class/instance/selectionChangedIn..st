updating
selectionChangedIn: aColumn
	aColumn hasSelection
		ifTrue: [self selected: aColumn]
		ifFalse: [self clearAfter: aColumn].
	self reclaimPanes.