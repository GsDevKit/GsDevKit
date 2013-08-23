testing
isActive
	^ (target isMethodNode or: [target isClassNode]) and: [requestor isSelected: target]