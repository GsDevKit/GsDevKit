confirmation
isRedefinition: aDefinition
	^ (self nameOfClassDefinedBy: aDefinition) = (self nameOfClassDefinedBy: template)