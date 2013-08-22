execution
execute
	requestor browser root versionMerger loadWithNameLike: nil.
	requestor browser close.
	self doAutoCommit