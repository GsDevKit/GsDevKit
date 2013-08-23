testing
isActive
	^ (target isKindOf: OBMethodCategoryNode)
		and: [target theClass inheritsFrom: TestCase]
		and: [requestor isSelected: target]