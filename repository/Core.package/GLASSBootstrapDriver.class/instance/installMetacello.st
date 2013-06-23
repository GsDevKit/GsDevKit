installing
installMetacello

	self autoCommitDuring: [
		self monticelloInstall: self metacelloFilenames.
		System commitTransaction ifFalse: [ ^self error: 'commit failed' ] ].