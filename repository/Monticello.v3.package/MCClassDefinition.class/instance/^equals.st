comparing
= aDefinition
	"see hasSimpleModsRelativeTo:, and keep it in synch"

    ^ (super = aDefinition)
        and: [superclassName = aDefinition superclassName
        and: [self traitCompositionString = aDefinition traitCompositionString
        and: [self classTraitCompositionString = aDefinition classTraitCompositionString
        and: [category asString = aDefinition category asString
        and: [ (self typeEquals: aDefinition type)
        and: [ self sortedVariables = aDefinition sortedVariables
        and: [comment = aDefinition comment]]]]]]]