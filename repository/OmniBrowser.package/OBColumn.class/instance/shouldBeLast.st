testing
shouldBeLast
	^ self hasSelection not or: [self nextMetaNode hasChildren not]