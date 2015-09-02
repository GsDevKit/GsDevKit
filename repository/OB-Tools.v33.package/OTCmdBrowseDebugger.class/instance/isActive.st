testing
isActive
	^ (requestor isSelected: target) and: [ (target isKindOf: OTContextNode) and: [ target theClass notNil ] ]