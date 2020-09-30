monticello installing
postGLASSInstallInitialization

	| repository |
	self metacelloGemStonePlatformClass initialize.
	repository := self mCDictionaryRepositoryClass new.
	self mCCacheRepositoryClass setDefault: repository.
	self mCPlatformSupportClass autoMigrate: true.
	self definedUndefinedSymbols.
	
	System commitTransaction ifFalse: [ ^self error: 'commit failed' ].