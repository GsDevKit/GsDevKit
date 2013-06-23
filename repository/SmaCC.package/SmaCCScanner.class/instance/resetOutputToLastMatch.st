private
resetOutputToLastMatch
	outputStream position: lastOutputStreamMatchPosition.
	lastOutputStreamMatchPosition == 1 
		ifTrue: 
			[lastMatchWasEmpty ifTrue: [self scannerError].
			lastMatchWasEmpty := true]
		ifFalse: [lastMatchWasEmpty := false]
