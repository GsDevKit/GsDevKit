testing
isActive
	^ target theClass ~= Object and: [requestor isSelected: target]