updating
subscribe
	self announcer 
		observe: OBSelectionChanged
		send: #selectionChanged:
		to: self