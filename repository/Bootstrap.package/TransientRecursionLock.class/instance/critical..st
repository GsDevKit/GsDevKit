Mutual Exclusion
critical: aBlock
	"Answer the result of evaluating aBlock while allowing the receiver's
	 semaphore to synchronize processing other processes."

	lockingProcess == Processor activeProcess ifTrue: [^aBlock value].
	self semaphore wait.
	lockingProcess := Processor activeProcess.
	^aBlock ensure: 
			[lockingProcess := nil.
			self semaphore signal]