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
              "We're within 2% of out of memory and cannot re-enable ."
              Transcript
                cr;
                show:
                    'Almost out of memory threshold ' , commitThreshold printString
                        ,
                          ' > 98. Unable to re-enable almost of out memory handler. Check log file for instance count report and Smalltalk stack.'.
              System commitTransaction
                ifFalse: [ nil error: 'AutoCommit failed' ] ] ].
      ex resume ].
  SessionTemps current
    at: #'CommitOnAlmostOutOfMemoryStaticException'
    put: handler.
  System signalAlmostOutOfMemoryThreshold: commitThreshold