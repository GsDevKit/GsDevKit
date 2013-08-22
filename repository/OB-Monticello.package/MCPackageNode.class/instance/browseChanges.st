accessing
browseChanges
	| patch |
	patch := self workingCopy changesRelativeToRepository: self repository.
	patch isNil ifTrue: [^ self].
	patch isEmpty
		ifTrue: [ 
			self workingCopy modified: false.
			OBInformRequest message: 'No changes' ]
		ifFalse: [ 
				self workingCopy modified: true.
				MCPatchTool openRoot: (MCPatchNode new patch: patch; label: self workingCopy packageName; yourself)]
