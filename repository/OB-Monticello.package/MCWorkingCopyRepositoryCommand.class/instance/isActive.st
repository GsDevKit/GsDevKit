as yet unclassified
isActive
	^ target hasWorkingCopy and: [target hasRepository] and: [requestor isSelected: target]