confirming
confirmScarySelector: aSelector
	| prompt |
	prompt := aSelector, ' is used in the existing class system.
Overriding it could cause serious problems.
Is this really what you want to do?'.

	^ OBConfirmationRequest
		prompt: prompt
		confirm: 'Override'