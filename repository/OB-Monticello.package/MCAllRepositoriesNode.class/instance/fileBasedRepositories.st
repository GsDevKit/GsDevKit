navigating
fileBasedRepositories
	^ self repositories select: [:ea | ea repository isKindOf: MCFileBasedRepository]