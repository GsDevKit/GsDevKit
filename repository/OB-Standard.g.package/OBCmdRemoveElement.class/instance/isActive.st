testing
isActive

	^ (target isKindOf: OBCollectionNode) and: [requestor hasSelection]