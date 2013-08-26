running
tearDown
	super tearDown.
	workingCopy unregister.
	self restoreMocks.
	SystemOrganizer new removeEmptyCategories.
	MCSnapshotTest organization removeEmptyCategories.
	GsPackagePolicy current authorInitials: savedInitials