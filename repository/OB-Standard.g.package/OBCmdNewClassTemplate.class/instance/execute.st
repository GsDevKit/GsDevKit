execution
execute
	| class definition |
	class := target theNonMetaClass.
	definition := (OBClassDefinition 
					environment: class environment 
					template: (Class template: class category)).
	requestor announce: (OBDefinitionChanged definition: definition)