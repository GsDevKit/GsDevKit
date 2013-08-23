Monticello
loadVersionNamed: versionName from: url

	| httpRepository version rg |
	httpRepository := MCHttpRepository
    		location: url
    		user: ''
    		password: ''.
	version := httpRepository loadVersionFromFileNamed: versionName.
	version load.
	rg := version workingCopy repositoryGroup.
	rg addRepository: httpRepository.
	^System commitTransaction