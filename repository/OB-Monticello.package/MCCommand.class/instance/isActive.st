testing
isActive
	^ (target isKindOf: self targetClass) and: [self targetMustBeSelected not or: [requestor isSelected: target]]