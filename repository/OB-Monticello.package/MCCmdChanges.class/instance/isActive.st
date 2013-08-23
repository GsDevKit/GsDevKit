testing
isActive
	^ target hasChanges and: [requestor isSelected: target]