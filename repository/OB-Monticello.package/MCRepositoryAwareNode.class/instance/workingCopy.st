testing
workingCopy
	^ MCWorkingCopy allManagers detect: [:ea | ea repositoryGroup = repositoryGroup] ifNone: []