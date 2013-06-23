initialization
initialize

	self enable.
	sessionMethodChangeCount := classChangeCount := 0.
	self refreshCache
