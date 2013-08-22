filtering
limitClassFor: aNode 
	^ limitClass ifNil: [self defaultLimitClass ] ifNotNil: [limitClass]