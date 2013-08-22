actions
refresh
	requestor announcer announce: OBRefreshRequired.
	self shouldAutoCommit 
		ifTrue: [ 
			false ifTrue: [(ObjectLogEntry debug: 'refresh' object: requestor) addToLog].
			self doAutoCommit ].