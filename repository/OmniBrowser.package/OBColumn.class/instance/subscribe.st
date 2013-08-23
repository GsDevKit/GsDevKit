updating
subscribe
	self announcer 
		observe: OBRefreshRequired send: #refresh: to: self;
		observe: OBNodeChanged send: #nodeChanged: to: self;
		observe: OBNodeDeleted send: #nodeDeleted: to: self;
		observe: OBChildrenChanged send: #childrenChanged: to: self;
		observe: OBSelectionChanged send: #selectionChanged: to: self.