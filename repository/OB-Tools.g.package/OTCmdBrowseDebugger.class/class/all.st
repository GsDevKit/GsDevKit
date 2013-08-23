instance-creation
all
	^ OrderedCollection new
		add: (self with: OBCmdBrowse);
		add: (self with: OBCmdBrowseHierarchy);
		add: (self with: OBCmdBrowseSenders);
		add: (self with: OBCmdBrowseImplementors);
		add: (self with: OBCmdBrowseInheritance);
		add: (self with: OBCmdBrowseProtocol);
		add: (self with: OBCmdBrowseReferences);
		add: (self with: OBCmdBrowseMethodVersions);
		add: (self with: OBCmdChaseVariables);
		add: (self with: OBCmdBrowseHierarchySenders);
		add: (self with: OBCmdBrowseHierarchyImplementors);		
		add: OBCmdBrowseSendersOfIt;
		add: OBCmdBrowseMethodsWithIt;
		add: OBCmdBrowseImplementorsOfIt;
		yourself