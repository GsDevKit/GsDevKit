testing
isActive
	^ (target isKindOf: OBClassNode)
		and: [requestor isSelected: target]
		