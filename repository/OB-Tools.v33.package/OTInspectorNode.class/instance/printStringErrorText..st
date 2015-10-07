private
printStringErrorText: exception

	^('<', (self printStringErrorType: exception), ' in printstring for class: ', self class name asString, '>') asText