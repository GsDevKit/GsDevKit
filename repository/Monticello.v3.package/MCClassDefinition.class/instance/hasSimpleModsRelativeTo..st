comparing
hasSimpleModsRelativeTo: aDefinition
	"simple mods are comment or category changes"
	
	^ (super = aDefinition)
		and: [(superclassName = aDefinition superclassName and: [
				self actualClass superclass == (Smalltalk at: superclassName asSymbol)])
		and: [self traitCompositionString = aDefinition traitCompositionString
            and: [self classTraitCompositionString = aDefinition classTraitCompositionString
		and: [ (self typeEquals: aDefinition type)
		and: [variables = aDefinition variables]]]]]