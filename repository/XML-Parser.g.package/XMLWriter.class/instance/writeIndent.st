private
writeIndent
	currentIndent ifNotNil: [
		currentIndent timesRepeat: [self stream nextPutAll: indentString]]