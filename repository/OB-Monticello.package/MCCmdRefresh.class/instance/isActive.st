as yet unclassified
isActive
	^ (target hasRepository or: [target hasVersion]) and: [requestor isSelected: target]