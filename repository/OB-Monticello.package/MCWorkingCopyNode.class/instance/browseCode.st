accessing
browseCode
	| patch |
	patch := self workingCopy definitionsAsChanges.
	patch isNil ifTrue: [^ self].
	MCCodeTool openRoot: (MCBrowseNode new patch: patch; label: self workingCopy packageName; yourself)
