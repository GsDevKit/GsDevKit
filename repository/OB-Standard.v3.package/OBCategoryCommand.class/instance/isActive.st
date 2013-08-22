as yet unclassified
isActive
	^ target hasOrganization 
		and: [target organization isClassOrganizer
			and: [ (requestor isSelected: target) not and: 
				[ requestor selectedNode isNil or: [ requestor selectedNode isCategoryNode ] ] ] ]