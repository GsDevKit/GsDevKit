driver
writeRelease: configurationClass version: versionString attributes: attributes extension: extension path: path repositoryPath: repositoryPath packingListPath: packingListPath

	self configuration: configurationClass.
	GLASSBootstrapMetacelloPlatform 
		useAttributes: attributes 
		while: [ | repositoryMap |
			repositoryMap := self 
				writeAllPackagesFor: versionString 
				packingListPath: packingListPath 
				to: repositoryPath.
			self
				writeInstallMasterFor: versionString 
				extension: extension
				path: path 
				repositoryMap: repositoryMap 
				repositoryPath: repositoryPath.
			self
				writeBootstrapMasterFor: versionString 
				extension: extension 
				path: path ]
