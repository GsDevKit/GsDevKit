metacello installing
installPackageSpec: projectClassName repository: httpRepositoryDescription

	| repositorySpec |
	repositorySpec := self useRepositoryDirectory
		ifTrue: [
			self metacelloMCProjectClass new repositorySpec
				description: self repositoryDirectory;
				type: 'directory';
				yourself ]
		ifFalse: [
			self metacelloMCProjectClass new repositorySpec
				description: httpRepositoryDescription;
				type: 'http';
				yourself ].
	(self metacelloMCProjectClass new packageSpec
		name: projectClassName;
		repository: repositorySpec
		yourself) load.
	System commitTransaction ifFalse: [ ^self error: 'commit failed' ].