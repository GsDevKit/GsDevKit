remote debugging
debugContinuation
	
	| procs logEntry |
	System needsCommit ifTrue: [ | result |
								result := self confirm: 'About to abortTransaction. Okay?'.
								result ~~ true ifTrue: [^self] ].
	System abortTransaction.
	procs := ObjectLogEntry objectLog select: [:ea | ea hasContinuation].
	procs size = 0 ifTrue: [ ^self inform: 'No continuations to debug' ].
	procs size == 1 
		ifTrue: [ logEntry := procs first ]
		ifFalse: [ | index |
			procs := procs sortBy: [:a :b | a hasTag ].
			logEntry := OBChoiceRequest 
				prompt: 'Select a continuation. Preceding continuations will be removed.' 
				labels: (procs collect: [:ea | 
					ea hasTag
						ifTrue: [ ea tag asString, '_', ea objectString ]
						ifFalse: [ ea objectString]])
				values: procs.
			logEntry == nil ifTrue: [ ^self ]].
	logEntry isResumable
		ifTrue: [ SelectedLogEntry := logEntry ]
		ifFalse: [ SelectedLogEntry := nil ].
	logEntry continuation value: #debug.