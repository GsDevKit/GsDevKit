private
loadClassDefinition: aDefinition
	[aDefinition isClassDefinition ifTrue:[aDefinition load].
	"make sure that the sessionMethodDictionary is updated correctly - bug38053"
	GsPackagePolicy current refreshSessionMethodDictionary	
	] on: Error do: [errorDefinitions add: aDefinition].
