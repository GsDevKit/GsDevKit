installing
installConfigurationPackage

	self autoCommitDuring: [
		self postGLASSInstallInitialization.
		self 
			installPackageSpec: self configurationPackageName 
			repository: self configurationRepository.
		self metacelloGemStonePlatformClass initialize.
		System commitTransaction ifFalse: [ ^self error: 'commit failed' ] ].