running
tearDown

	super tearDown.
	(diff isNil or: [diff isEmpty not])
		 ifTrue: [expected updatePackage: self mockPackage]