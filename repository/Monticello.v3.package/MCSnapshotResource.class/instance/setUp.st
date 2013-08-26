as yet unclassified
setUp
	super setUp.
	snapshot := self class takeSnapshot.
	saveCache := MCCacheRepository default.
	MCCacheRepository setDefault: self monticelloTestSaveCacheRepository.