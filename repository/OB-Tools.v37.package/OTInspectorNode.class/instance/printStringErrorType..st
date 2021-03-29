private
printStringErrorType: exception

	^(exception isKindOf: Halt)
		ifTrue: [ 'halt' ]
		ifFalse: [ 'error' ]