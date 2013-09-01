mocks
mockClass: className super: superclassName classVarNames: classVarNames
	^ MCClassDefinition
		name:  className
		superclassName:  superclassName
		category: self mockCategoryName
		instVarNames: #()
		classVarNames: classVarNames
		poolDictionaryNames: #()
		classInstVarNames: #()
		type: #normal
		comment: 'This is a comment'
		commentStamp: 'tester-dkh 1/1/2000 00:00'