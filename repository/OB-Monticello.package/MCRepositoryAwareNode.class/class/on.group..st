instance creation
on: aRepository group: aRepositoryGroup
	^ self new
		repository: aRepository;
		repositoryGroup: aRepositoryGroup;
		yourself