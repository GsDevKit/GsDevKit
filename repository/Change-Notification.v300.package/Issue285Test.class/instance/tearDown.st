running
tearDown

	super tearDown.
	Smalltalk at: self testClassName ifPresent: [:cl | cl removeFromSystem ]