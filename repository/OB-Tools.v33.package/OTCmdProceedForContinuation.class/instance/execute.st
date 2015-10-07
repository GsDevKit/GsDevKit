execution
execute

	| logEntry |
	(logEntry := OTRemoteDebugger selectedLogEntry) ~~ nil
		ifTrue: [
			logEntry resumption: target context process.
			OTRemoteDebugger clearSelectedLogEntry.
			System commitTransaction 
				ifFalse: [ self inform: 'Cannot proceed from this remotely debugged process' ].
			OBCloseRequest signal: requestor browser.
			"abort transaction to 'forget' about fact that <target context process> is terminated
			 when the debugger is closed"
			System abortTransaction]
		ifFalse: [ self inform: 'Cannot proceed from a remotely debugged process' ].
