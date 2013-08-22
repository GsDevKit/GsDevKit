as yet unclassified
annotationString

	| def |
	def := self operation definition.
	def isMethodDefinition ifFalse: [ ^'' ].
	^(MCDefinitionAnnotationRequest onDefinition: def) getAnnotations