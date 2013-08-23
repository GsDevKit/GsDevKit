testing
isActive

	^ target hasVersions and: [requestor isSelected: target]