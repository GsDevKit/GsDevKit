testing
isActive
	^ (requestor isSelected: target) and: [ target isKindOf: OTProcessNode ]