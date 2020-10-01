accessing
acquireObjectLogLock

	| queue |
	queue := self objectQueue.
	System writeLock: queue
		ifDenied: [ ^false ]
		ifChanged: [
			System addToCommitOrAbortReleaseLocksSet: queue. 
			^false ].
	System addToCommitOrAbortReleaseLocksSet: queue.
	^true