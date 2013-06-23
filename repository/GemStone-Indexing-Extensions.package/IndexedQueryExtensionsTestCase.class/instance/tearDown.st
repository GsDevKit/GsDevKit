running
tearDown

	super tearDown.
	nscs do: [:nsc | nsc removeAllIndexes ].
	queryValue := nil