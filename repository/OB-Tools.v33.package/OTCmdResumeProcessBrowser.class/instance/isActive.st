testing
isActive
	^ super isActive and: [ target process isSuspended ]