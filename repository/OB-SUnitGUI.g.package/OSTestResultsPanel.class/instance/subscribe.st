updating
subscribe
	self announcer 
		observe: OBSelectionChanged
		send: #selectionChanged:
		to: self;

		observe: OBRefreshRequired
		send: #refresh:
		to: self