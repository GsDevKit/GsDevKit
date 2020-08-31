mocks
mockClass: className super: superclassName traitComposition: tc classTraitComposition: ctc
	^ MCClassDefinition
		name:  className
		superclassName:  superclassName
		traitComposition: tc
		classTraitComposition: ctc
		category: self mockCategoryName
		instVarNames: #()
		classVarNames: #()
		poolDictionaryNames: #()
		classInstVarNames: #()
		type: #normal
		comment: (self commentForClass: className)
		commentStamp: (self commentStampForClass: className)