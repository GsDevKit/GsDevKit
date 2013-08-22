as yet unclassified
isActive

	| def |
	def := requestor getDefinition.
	^((def isKindOf: OBMethodDefinition) or: [def isKindOf: OGContextNode])
		and: [GsMethod _allMethodBreakpoints isEmpty not]