private
printStringErrorText: exception

	^ ('<', (self printStringErrorType: exception), ' in printString: evaluate "(self at: ', self key printString, ') printString" to debug>') asText.