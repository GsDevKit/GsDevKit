execution
execute

	MCRepositoryGroup default repositoriesDo: [:rep | rep flushCache ].
	self doAutoCommit