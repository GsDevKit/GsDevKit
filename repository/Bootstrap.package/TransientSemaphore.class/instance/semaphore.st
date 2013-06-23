private
semaphore

    semaphore == nil
        ifTrue: [
			semaphore := Semaphore new.
			self forMutualExclusion ifTrue: [ semaphore signal ].
			"SeasidePlatformSupport addTransient: self."
        ].
    ^semaphore