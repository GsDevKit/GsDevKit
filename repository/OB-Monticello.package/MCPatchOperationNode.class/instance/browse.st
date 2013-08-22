as yet unclassified
browse

	| def |
	def := self operation definition.
	def isOrganizationDefinition ifTrue: [ ^nil ].
	def isClassDefinition
		ifTrue: [ ^def actualClass browse ].
	(OBMessageNode 
		on: def selector 
		inMethod: def selector 
		inClass: def actualClass) browse