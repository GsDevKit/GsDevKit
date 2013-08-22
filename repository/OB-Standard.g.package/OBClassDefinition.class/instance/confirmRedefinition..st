confirmation
confirmRedefinition: definition
	| newName prompt |
	newName := self nameOfClassDefinedBy: definition.
	prompt := newName, ' is an existing class in this system.
Redefining it might cause serious problems.
Is this really what you want to do?'.
	^ OBConfirmationRequest
		prompt: prompt
		confirm: 'Redefine'