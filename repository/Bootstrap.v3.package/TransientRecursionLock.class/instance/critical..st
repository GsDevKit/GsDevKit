*bootstrap-mutual-exclusion
critical: aBlock
	"Answer the result of evaluating aBlock while allowing the receiver's
	semaphore to synchronize processing other processes.

	WARNING: do not cache semaphore reference in tems. The variable 
		contexts for this method may be persisted"

	| proc |
	lockingProcess == (proc := Processor activeProcess) ifTrue: [^ aBlock value ].
	self semaphore wait.
	lockingProcess := proc .
	^ aBlock ensure: [
		lockingProcess := nil.
		self semaphore signal ]
