running
tearDown
	Smalltalk at: 'MCMockClassT' ifPresent: [:c | c removeFromSystem].
	super tearDown.
