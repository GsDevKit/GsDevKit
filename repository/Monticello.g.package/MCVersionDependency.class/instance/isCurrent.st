testing
isCurrent
	^ self isFulfilled and: [package workingCopy modified not]