operations
patchMessageChanges
	| changes parentInfo parentSnapshot |
	parentInfo := self ancestors
		ifEmpty: [nil]
		ifNotEmpty: [self ancestors first].
	parentSnapshot := 	self findSnapshotWithVersionInfo: parentInfo.
	changes := package snapshot patchRelativeToBase: parentSnapshot.
	^ (MCPatchMessage new patch: changes) message