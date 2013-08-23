callbacks
text
	^ self withDefinitionDo: [:def | def text] ifNil: ['']