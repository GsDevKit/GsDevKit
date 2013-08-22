as yet unclassified
operations
	^ self merger operations collect: [:ea | MCPatchOperationNode new operation: ea]