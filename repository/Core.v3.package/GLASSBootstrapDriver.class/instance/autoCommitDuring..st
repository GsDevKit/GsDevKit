auto commit
autoCommitDuring: aBlock
  [ | commitThreshold |
    commitThreshold :=  75.
    [ 
      System signalAlmostOutOfMemoryThreshold: commitThreshold . 
      aBlock value
    ] onException: AlmostOutOfMemory do:[ :ex |
      System commitTransaction ifFalse: [ self error: 'AutoCommit failed' ].
      System _vmMarkSweep.
      (System _tempObjSpacePercentUsedLastMark < commitThreshold) ifTrue: [
        "We dropped below the threshold reenable the signal"
        System enableAlmostOutOfMemoryError 
      ].
      ex resume  "continue execution" 
    ]
  ] ensure:[
    System signalAlmostOutOfMemoryThreshold: -1 "disable AlmostOOM signal"
  ].
