command
isActive
	^(target isKindOf: OBMethodNode) and: [(requestor isSelected: target) "and: [target hasHierarchyImplementors]"]