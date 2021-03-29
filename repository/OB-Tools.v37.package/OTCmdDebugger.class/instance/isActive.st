testing
isActive
	^ (target isKindOf: OTContextNode) and: [ requestor isSelected: target ]