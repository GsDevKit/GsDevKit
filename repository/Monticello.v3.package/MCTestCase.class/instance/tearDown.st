running
tearDown
	
	self clearPackageCache.
	MCCacheRepository setDefault: saveCache.
	MCPlatformSupport autoMigrate: saveAutoMigrate.
	self monticelloPackage modified: (monticelloPackageModified ifNil: [false]).
	MCPlatformSupport autoCommit 
		ifTrue: [ System commitTransaction ifFalse: [ self error: 'Commit during MCTest run failed' ]].
	super tearDown.