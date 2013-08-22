as yet unclassified
theClass

	| def |
	def := self operation definition.
	def isOrganizationDefinition ifTrue: [ ^nil ].
	^def actualClass