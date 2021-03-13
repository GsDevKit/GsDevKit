testing
isActive

	| def |
	def := requestor getDefinition.
	^((def isKindOf: OBMethodDefinition) or: [def isKindOf: OGContextNode]) and: [requestor getDefinition theClass ~~ nil ]