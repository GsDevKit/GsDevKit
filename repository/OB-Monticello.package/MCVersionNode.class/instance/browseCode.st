accessing
browseCode
	| patch |
	patch := self version definitionsAsChanges.
	patch isNil ifTrue: [^ self].
	MCCodeTool openRoot: (MCBrowseNode new patch: patch; label: self version info name; yourself)