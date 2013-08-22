preferences
doAutoCommit

	| result |
	MCPlatformSupport autoCommit ifFalse: [ ^true ].
	result := System commitTransaction.
	false 
		ifTrue: [
			(ObjectLogEntry debug: 'autoCommit' object: result) addToLog.
			^System commitTransaction].
	^result