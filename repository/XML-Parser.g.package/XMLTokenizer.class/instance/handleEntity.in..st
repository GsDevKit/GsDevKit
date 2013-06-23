entities
handleEntity: referenceString in: parsingContext 

	| entity entityValue |
	entity _ self entity: referenceString.
	entityValue _ entity valueForContext: parsingContext.
	(self class isCharEscape: entityValue)
		ifTrue: [entityValue _ entity reference].
	self pushStream: (ReadStream on: entityValue asString)