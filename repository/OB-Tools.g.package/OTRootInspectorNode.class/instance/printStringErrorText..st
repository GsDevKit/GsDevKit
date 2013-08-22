private
printStringErrorText: exception

	^('<', (self printStringErrorType: exception), ' in printString: evaluate "self printString" to debug>') asText.