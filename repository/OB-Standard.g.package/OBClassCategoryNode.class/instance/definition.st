displaying
definition
	^ OBClassDefinition 
		environment: environment 
		template: ((environment at: #Class) template: name)