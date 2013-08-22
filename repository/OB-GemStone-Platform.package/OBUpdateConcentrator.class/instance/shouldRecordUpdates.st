testing
shouldRecordUpdates

	self canRecordUpdates ifFalse: [ ^false ].
	^self updateStack isEmpty not