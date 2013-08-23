testing
isActive
	^ super isActive and: [target canBeBrowsed]