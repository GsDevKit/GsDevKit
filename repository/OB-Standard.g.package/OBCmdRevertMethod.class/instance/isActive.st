testing
isActive

	^ (target isKindOf: OBMethodVersionNode) and: [requestor isSelected: target]