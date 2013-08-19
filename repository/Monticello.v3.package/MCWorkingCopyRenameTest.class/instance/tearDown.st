running
tearDown
	super tearDown.
	workingCopy unregister.
	SystemOrganizer new removeEmptyCategories.
	MCSnapshotTest organization removeEmptyCategories.
	GsPackagePolicy current authorInitials: savedInitials