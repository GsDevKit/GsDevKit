testing
isActive
	^ (requestor isSelected: target) and: [ (target isKindOf: OTInspectorNode) and: [ target theClass notNil ] ]