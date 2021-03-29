execution
execute
	| semaphore |
	semaphore := target process suspendingList.
	(semaphore isKindOf: Semaphore) 
		ifFalse: [ ^ self ].
	[ semaphore signal ] forkAndWait.
	self update