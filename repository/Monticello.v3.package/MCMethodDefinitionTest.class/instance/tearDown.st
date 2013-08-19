running
tearDown

	self restoreMocks.
	(MCWorkingCopy forPackage: (MCPackage named: 'FooBarBaz')) unregister.
	MCMockPackageInfo new mcPackage workingCopy unregister.
	MCPlatformSupport
		compile: self class
		source: 'override ^ 1'
		classified: 'mocks'
		withStamp: overrideTimestamp.
	SystemOrganizer new removeEmptyCategories.
	self ownPackage modified: isModified.
	super tearDown
