private
templateDefinition
	^ OBClassDefinition environment: self currentEnvironment
		template: (Class templateForSubclassOf: self currentClass
				category: self currentCategory)