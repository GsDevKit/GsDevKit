driver
writeAllPackagesFor: aVersionString packingListPath: packingListPath to: aRepositoryPath

	| project version loader repository repositoryMap dir |
	project := ConfigurationOfGLASS project.
	version := project version: aVersionString.
	loader := version versionSpec loader.
	repository := MCServerDirectoryRepository new directory: (dir := ServerFileDirectory on: aRepositoryPath).
	dir assureExistence.
	repositoryMap := self allPackageFilenames: aVersionString copyingTo: repository in: dir.
	self autoEditPackingList: packingListPath from: (repositoryMap keys collect: [:each | each file ]).
	^repositoryMap