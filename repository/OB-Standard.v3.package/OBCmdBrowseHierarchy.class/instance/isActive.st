testing
isActive
	^ ((target isKindOf: OBClassAwareNode) or: [(target respondsTo: #theClass) and: [target theClass notNil]])
		and: [requestor isSelected: target]