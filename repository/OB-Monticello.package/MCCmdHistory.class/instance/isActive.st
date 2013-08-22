testing
isActive
	^ target hasAncestry and: [requestor isSelected: target]