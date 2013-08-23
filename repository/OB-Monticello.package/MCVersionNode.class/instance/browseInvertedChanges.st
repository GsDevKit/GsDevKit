accessing
browseInvertedChanges
	| patch |
	patch _ self version invertedChanges.
	patch isEmpty
		ifTrue: [OBInformRequest message: 'No changes' ]
		ifFalse: [MCPatchTool openRoot: (MCPatchNode new patch: patch; label: self version info name; yourself)]