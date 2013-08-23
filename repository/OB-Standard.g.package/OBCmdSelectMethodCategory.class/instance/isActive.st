testing
isActive
	^ (target isKindOf: OBMethodNode)
		and: [requestor isSelected: target]