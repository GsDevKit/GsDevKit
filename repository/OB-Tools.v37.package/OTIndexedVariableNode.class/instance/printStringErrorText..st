private
printStringErrorText: exception

	^('<', (self printStringErrorType: exception), ' in printString: evaluate "(self basicAt: ', self index printString, ') printString" to debug>') asText.