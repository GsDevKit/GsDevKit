accessing
browseInvertedChanges
	| patch |
	patch := self workingCopy invertedChangesRelativeToRepository: self repository.
	patch isNil ifTrue: [^ self].
	patch isEmpty
		ifTrue: [ 
			self workingCopy modified: false.
			OBInformRequest message: 'No changes' ]
		ifFalse: [ 
				self workingCopy modified: true.
				MCInvertedPatchTool openRoot: (MCPatchNode new patch: patch; label: self workingCopy packageName; yourself)]
