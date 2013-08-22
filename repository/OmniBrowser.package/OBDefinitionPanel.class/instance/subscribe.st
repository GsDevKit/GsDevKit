updating
subscribe
	self announcer 
		observe: OBAboutToChange
		send: #aboutToChange:
		to: self;
		
		observe: OBAboutToChangeSilently
		send: #aboutToChangeSilently:
		to: self;
		
		observe: OBSelectionChanged
		send: #selectionChanged:
		to: self;
				
		observe: OBRefreshRequired
		send: #refresh:
		to: self;
					
		observe: OBDefinitionChanged
		send: #definitionChanged: 
		to: self.