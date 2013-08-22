accessing
browseChanges
	| patch |
	patch _ self version changes.
	patch isEmpty
		ifTrue: [OBInformRequest message: 'No changes' ]
		ifFalse: [MCPatchTool openRoot: (MCPatchNode new patch: patch; label: self version info name; yourself)]