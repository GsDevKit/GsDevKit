testing
isActive
	^ (target isKindOf: OBClassAwareNode)
		and: [requestor isSelected: target]