installing
installConfigurationPackage

	self autoCommitDuring: [
		self 
			installPackageSpec: self configurationPackageName 
			repository: self configurationRepository.

		System commitTransaction ifFalse: [ ^self error: 'commit failed' ] ].