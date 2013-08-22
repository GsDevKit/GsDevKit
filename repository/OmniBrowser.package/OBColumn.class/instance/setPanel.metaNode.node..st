initializing
setPanel: aPanel metaNode: aMetanode node: aNode 
	switch _ OBSwitch inColumn: self.
	panel _ aPanel.
	aNode metaNode: aMetanode.
	self basicParent: aNode.
	self clearSelection.
	self subscribe.