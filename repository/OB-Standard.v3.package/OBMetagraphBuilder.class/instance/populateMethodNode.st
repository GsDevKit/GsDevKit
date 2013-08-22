private
populateMethodNode
	self methodMetaNode 
		ancestrySelector: #isDescendantOfMethod:;
		addFilter: OBInheritanceFilter new