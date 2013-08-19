as yet unclassified
checkCacheDirectory
	default notNil and: [default cacheDirectoryExists ifFalse: [default := nil]]