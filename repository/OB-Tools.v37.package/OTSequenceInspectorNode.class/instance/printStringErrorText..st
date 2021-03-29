private
printStringErrorText: exception

	^('<', (self printStringErrorType: exception), ' in printString: evaluate "(self at: ', self index printString, ') printString" to debug>') asText.