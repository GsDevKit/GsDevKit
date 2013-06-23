driver
release: configurationClass version: versionString path: path repositoryPath: repositoryPath packingListPath: packingListPath

	self extensionAndAttributes do: [:ar | | extension attributes |
		extension := ar at: 1.
		attributes := ar at: 2.
		Transcript cr; show: 'Writing release artifacts for ', extension printString.
		self 
			writeRelease: configurationClass 
			version: versionString 
			attributes: attributes 
			extension: extension 
			path: path 
			repositoryPath: repositoryPath 
			packingListPath: packingListPath.
		Transcript cr; show: 'done with ', extension printString.].
		Transcript cr; show: '...finished'.
