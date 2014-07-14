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
          commitThreshold < 98
            ifTrue: [ System signalAlmostOutOfMemoryThreshold: commitThreshold ]
            ifFalse: [ 
              "we're with 10% of out of memory, commit instance counts structure 
               into objectlog for posterity."
              (ObjectLogEntry
                trace: 'almost out of memory: ' , commitThreshold printString
                object:
                  ((System _vmInstanceCounts: 3)
                    sort: [ :a :b | (a value at: 2) > (b value at: 2) ]))
                addToLog.
              System commitTransaction
                ifFalse: [ nil error: 'AutoCommit failed' ] ] ].
      ex resume ].
  SessionTemps current
    at: #'CommitOnAlmostOutOfMemoryStaticException'
    put: handler.
  System signalAlmostOutOfMemoryThreshold: commitThreshold