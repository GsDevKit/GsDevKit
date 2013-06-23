mcz installing
postMczInstallInitialization

	| repository |
	repository := self mCDictionaryRepositoryClass new.
	self mCCacheRepositoryClass setDefault: repository.
	self mCPlatformSupportClass autoMigrate: true.
	
	System commitTransaction ifFalse: [ ^self error: 'commit failed' ].