as yet unclassified
isActive

	| def cm |
	def := requestor getDefinition.
	(((def isKindOf: OBMethodDefinition) or: [def isKindOf: OGContextNode]) and: [ super isActive ]) ifFalse: [ ^false ].
	cm := self getCompiledMethod.
	^cm ==nil 
		ifTrue: [ false ]
		ifFalse: [ (cm _allBreakpoints) ~~ nil ]