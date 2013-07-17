deploying
deploy: deployNoArgBlock

	self mcPlatformSupportDo: [	
		| deploymentDict |
		[ 
			self atomicInstall
				ifTrue: [ deploymentDict := self collectGlobalsAndExtensionMethodsDuring: deployNoArgBlock value]
				ifFalse: [ deployNoArgBlock value ]] 
			on: Warning 
			do: [:ex |
				Transcript cr; show: ex description.
				ex resume ].
		deploymentDict ifNotNil: [ self error: 'Atomic Install feature not functional' ].
		self  doBulkMigrate. "cleans up class history"
		self cleanUndefinedSymbols ].
	self commit.
