instance-creation
all
	^ OrderedCollection new
		add: (self with: OBCmdBrowse);
		add: (self with: OBCmdBrowseHierarchy);
		add: (self with: OBCmdBrowseProtocol);
		add: (self with: OBCmdBrowseReferences);
		add: (self with: OBCmdChaseVariables);
		yourself