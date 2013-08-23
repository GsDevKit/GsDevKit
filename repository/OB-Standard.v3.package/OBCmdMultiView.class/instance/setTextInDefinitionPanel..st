utils
setTextInDefinitionPanel: aTextOrString 

	(requestor announce: OBAboutToChange) isVetoed
		ifFalse: [requestor
				announce: (OBDefinitionChanged definition: (OBReadOnlyDefinition new setText: aTextOrString;
							 yourself))]