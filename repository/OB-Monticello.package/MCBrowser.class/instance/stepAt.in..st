morphic
stepAt: milliseconds in: aSystemWindow
	hasChanges ifTrue: [self signalRefresh].
	self clearChanges