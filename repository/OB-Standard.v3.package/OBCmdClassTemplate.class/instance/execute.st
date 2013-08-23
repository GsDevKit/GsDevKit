execution
execute
	| definition |
	definition := self templateDefinition.
	definition 
		ifNotNil: [requestor announce: (OBDefinitionChanged definition: definition)]