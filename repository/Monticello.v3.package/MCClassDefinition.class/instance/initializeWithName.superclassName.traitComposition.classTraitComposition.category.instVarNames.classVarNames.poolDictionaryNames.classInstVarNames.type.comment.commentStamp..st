initializing
initializeWithName: nameString
superclassName: superclassString
traitComposition: traitCompositionString
classTraitComposition: classTraitCompositionString
category: categoryString 
instVarNames: ivarArray
classVarNames: cvarArray
poolDictionaryNames: poolArray
classInstVarNames: civarArray
type: typeSymbolOrArray
comment: commentString
commentStamp: stampStringOrNil
    name := nameString asSymbol.
    superclassName := superclassString ifNil: ['nil'] ifNotNil: [superclassString asSymbol].
    traitComposition := traitCompositionString.
    classTraitComposition := classTraitCompositionString.
    category := categoryString.
    type := typeSymbolOrArray .
    comment := commentString withSqueakLineEndings.
    commentStamp := stampStringOrNil ifNil: [self defaultCommentStamp].
    variables := OrderedCollection  new.
    self addVariables: ivarArray ofType: MCInstanceVariableDefinition.
    self addVariables: cvarArray ofType: MCClassVariableDefinition.
    self addVariables: poolArray ofType: MCPoolImportDefinition.
    self addVariables: civarArray ofType: MCClassInstanceVariableDefinition.