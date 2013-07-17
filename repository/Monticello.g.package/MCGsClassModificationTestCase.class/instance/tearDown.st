running
tearDown

	self restoreMocks.
	Smalltalk at: #Issue343TestClass ifPresent: [:cl | cl removeFromSystem].
	super tearDown.
