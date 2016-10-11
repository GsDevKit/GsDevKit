private
objectLogEntries: shouldLock
  shouldLock
    ifTrue: [ 
      System
        writeLock: ObjectLogEntry objectQueue
        ifDenied: [ 
          Transcript show: 'ObjectLogEntry objectQueue lock denied'.
          ^ nil ]
        ifChanged: [ 
          System addToCommitOrAbortReleaseLocksSet: ObjectLogEntry objectQueue.
          Transcript show: 'ObjectLogEntry objectQueue lock dirty'.
          ^ nil ].
      System addToCommitOrAbortReleaseLocksSet: ObjectLogEntry objectQueue ].
  ^ ObjectLogEntry objectLog