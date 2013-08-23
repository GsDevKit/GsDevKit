comparing
= aDefinition
	"see hasSimpleModsRelativeTo:, and keep it in synch"

    ^ (super = aDefinition)
        and: [superclassName = aDefinition superclassName
        and: [
		((self respondsTo: #traitCompositionString) not
		or: [self traitCompositionString = aDefinition traitCompositionString
	  	      and: [self classTraitCompositionString = aDefinition classTraitCompositionString]])
        and: [category asString = aDefinition category asString
        and: [ (self typeEquals: aDefinition type)
        and: [variables = aDefinition variables
        and: [comment = aDefinition comment]]]]]]
