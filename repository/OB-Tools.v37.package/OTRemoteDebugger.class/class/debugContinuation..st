remote debugging
debugContinuation: aLogEntry
	
	aLogEntry isResumable
		ifTrue: [ SelectedLogEntry := aLogEntry ]
		ifFalse: [ SelectedLogEntry := nil ].
	aLogEntry continuation value: #debug.