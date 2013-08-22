execution
execute
	| version |
	version := target version.
	version load.
	version workingCopy repositoryGroup addRepository: target repository.
	self refresh