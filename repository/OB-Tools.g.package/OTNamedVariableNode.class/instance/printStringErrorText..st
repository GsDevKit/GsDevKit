private
printStringErrorText: exception

	^('<', (self printStringErrorType: exception), ' in printString: evaluate "(self instVarAt: ', self index printString, ') printString" to debug>') asText.