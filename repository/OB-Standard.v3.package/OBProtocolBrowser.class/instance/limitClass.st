accessing
limitClass
	^ limitClass 
		ifNil: [self defaultLimitClass]
		ifNotNil: [limitClass]