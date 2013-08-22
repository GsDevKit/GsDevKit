private
templateDefinition
	^ OBClassDefinition environment: self currentEnvironment
		template: (Class template: self currentCategory)