file services
loadVersionFile: fileName
	| version |
	version := self versionFromFile: fileName.
	version workingCopy repositoryGroup addRepository:
		(MCDirectoryRepository new directory:
			(MCPlatformSupport fileDirectoryFor: fileName)).
	version load.