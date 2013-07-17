almost out of memory handling
installAlmostOutOfMemoryStaticHandler: commitThreshold

  | handler |
  handler := AlmostOutOfMemory addDefaultHandler: [ :ex |
      "Exception caught, do a commit."
      System commitTransaction ifFalse:[ nil error: 'AutoCommit failed' ].
      System _vmMarkSweep.  
     (System _tempObjSpacePercentUsedLastMark < commitThreshold) ifTrue: [
        "We dropped below the threshold, reenable the signal"
        System enableAlmostOutOfMemoryError 
      ].
      ex resume
  ].
  SessionTemps current at: #CommitOnAlmostOutOfMemoryStaticException 
			put: handler.
  System signalAlmostOutOfMemoryThreshold: commitThreshold
