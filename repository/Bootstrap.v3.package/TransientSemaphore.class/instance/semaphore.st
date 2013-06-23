*bootstrap-private
semaphore
   | sem |
   (sem := semaphore) ifNil:[
      sem := Semaphore new.
      semaphore := sem .
      self forMutualExclusion ifTrue: [ sem signal ].
      "SeasidePlatformSupport addTransient: self."
    ].
    ^ sem
