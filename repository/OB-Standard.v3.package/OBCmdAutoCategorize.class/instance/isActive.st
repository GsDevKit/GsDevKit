testing
isActive
	^ target hasOrganization and: 
		[ target organization isClassOrganizer and: [ (requestor isSelected: target) not ] ]