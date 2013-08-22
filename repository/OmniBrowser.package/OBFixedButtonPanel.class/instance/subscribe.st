callbacks
subscribe
	self announcer
		observe: OBSelectionChanged
		send: #selectionChanged:
		to: self.
	self announcer
		observe: OBNodeChanged
		send: #selectionChanged:
		to: self.