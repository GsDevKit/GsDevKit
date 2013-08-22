updating
subscribe
	self announcer
		observe: OBSelectingNode send: #selectNode: to: self;
		observe: OBSelectionChanged send: #selectionChanged: to: self;
		observe: OBNodeCreated send: #selectNode: to: self;
		observe: OBNodeDeleted send: #nodeDeleted: to: self.