utilities
listInstances: anArray

| reportArr res |
reportArr := SystemRepository listInstances: anArray 
			limit: 0 
			toDirectory: nil
			withMaxThreads: 1 "limit to a single thread for now ... should be configurable in the future"
			maxCpuUsage: 95.
res := Array new: anArray size .
1 to: anArray size do:[:j |
  res at:j put: (reportArr at: j * 2 ) . "array of instances"
].
^ res