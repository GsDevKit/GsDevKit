testing
isActive
	^ ((target hasWorkingCopy and: [target hasRepository]) or: [ target canAddRepository]) and: [requestor isSelected: target]