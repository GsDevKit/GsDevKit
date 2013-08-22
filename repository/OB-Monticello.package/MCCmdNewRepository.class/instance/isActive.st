testing
isActive
	^ super isActive and: [target canAddRepository]