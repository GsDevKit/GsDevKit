updating
recordUpdatesDuring: aBlock

	self canRecordUpdates ifFalse: [ ^aBlock value ].
	self updateStack addLast: Array new.
	aBlock ensure: [ self triggerUpdates: self updateStack removeLast ]