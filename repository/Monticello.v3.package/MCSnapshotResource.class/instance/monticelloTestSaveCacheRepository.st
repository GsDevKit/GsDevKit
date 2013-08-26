as yet unclassified
monticelloTestSaveCacheRepository

	^ MCCacheRepository new
		directory: (MCPlatformSupport defaultDirectoryNamed: 'mc-testing');
		yourself