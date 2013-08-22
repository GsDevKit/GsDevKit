remote debugging
continuationsFromObjectLog

	| logEntries answer |
	System abortTransaction.
	logEntries := ObjectLogEntry objectLog select: [:ea | ea hasContinuation].
	logEntries := logEntries sortBy: [:a :b | a hasTag ].
	answer := Array new.
	logEntries do: [:logEntry | | label |
		label := logEntry hasTag
			ifTrue: [ logEntry tag asString, '_', logEntry objectString ]
			ifFalse: [ logEntry objectString].
		answer add: label; add: logEntry asOop asString].
	^self stringFromCollection: answer