initializing
setPanel: aPanel 
	panel _ aPanel.
	switch _ OBSwitch inColumn: self.
	self subscribe.
	self listHolder clearAll