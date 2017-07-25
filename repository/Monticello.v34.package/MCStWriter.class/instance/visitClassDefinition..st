visiting
visitClassDefinition: definition
	self writeClassDefinition: definition.
	(definition hasClassInstanceVariables or: [definition hasClassTraitComposition])
		ifTrue: [self writeMetaclassDefinition: definition].
	definition hasComment ifTrue: [self writeClassComment: definition].