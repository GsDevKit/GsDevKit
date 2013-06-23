Mutual Exclusion
critical: aBlock
	"inline of Semaphore>>critical: to avoid having instance of Semaphore 
	  on stack if persisted as part of a continuation"
	
	| value |
	self semaphore wait.
	value := aBlock ensure: [ self semaphore signal ].
	^value
