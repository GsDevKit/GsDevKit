accessing
definition
	^ OBClassDefinition 
		environment: self theClass environment 
		template: self theNonMetaClass ogDefinition