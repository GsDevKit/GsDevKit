command
isActive
	^ target hasSelector and: [requestor isSelected: target]