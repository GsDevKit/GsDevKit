almost out of memory handling
installAlmostOutOfMemoryStaticHandler: initialCommitThreshold
  | handler commitThreshold |
  commitThreshold := initialCommitThreshold.
  handler := AlmostOutOfMemory
    addDefaultHandler: [ :ex | 
      "Exception caught, do a commit."
      System commitTransaction
        ifFalse: [ nil error: 'AutoCommit failed' ].
      System _vmMarkSweep.
      System _tempObjSpacePercentUsedLastMark < commitThreshold
        ifTrue: [ 
          "We dropped below the threshold, reenable the signal"
          System enableAlmostOutOfMemoryError ]
        ifFalse: [ 
          "commit and mark sweep did drop us below threshold. cut threshold in half 
           and try again. Record an entry in the object log "
          commitThreshold := (100 - commitThreshold) // 2 + commitThreshold.
          (commitThreshold > 0 and: [ commitThreshold < 98 ])
            ifTrue: [ System signalAlmostOutOfMemoryThreshold: commitThreshold ]
            ifFalse: [ 
              "We're within 2% of out of memory and cannot re-enable AlmostOutOfMemoryError, rely on GS_DEBUG_VMGC_VERBOSE_OUTOFMEM to record instance count report and Smalltalk stack to gem log. https://github.com/GsDevKit/GsDevKit/issues/90"
              "set threshold to 110 and hope for the best"
              commitThreshold := 110 ] ].
      ex resume ].
  SessionTemps current
    at: #'CommitOnAlmostOutOfMemoryStaticException'
    put: handler.
  System signalAlmostOutOfMemoryThreshold: commitThreshold