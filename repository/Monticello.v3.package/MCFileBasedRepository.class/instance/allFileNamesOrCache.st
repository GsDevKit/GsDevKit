private-files
allFileNamesOrCache
	^ allFileNames ifNil: [self allFileNames]