instance creation
initialize
	self setupRandom.
	semaphoreForGenerator := Semaphore forMutualExclusion.