almost out of memory handling
uninstallAlmostOutOfMemoryStaticHandler

  System signalAlmostOutOfMemoryThreshold: -1.
  (SessionTemps current 
     removeKey: #CommitOnAlmostOutOfMemoryStaticException 
      otherwise: nil) ifNotNil:[ :assoc | 
        Exception removeStaticException: assoc value
   ].