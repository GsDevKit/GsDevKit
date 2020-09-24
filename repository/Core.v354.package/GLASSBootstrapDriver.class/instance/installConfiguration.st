installing
installConfiguration

	self autoCommitDuring: [
		self postGLASSInstallInitialization.
		self 
			installProject: self configurationClass 
			version: self configurationVersionString 
			loads: self configurationPackageLoads.

		self repositoryMap associationsDo: [:assoc | | wc |
			wc := [ (self goferPackageReferenceClass name: assoc key) workingCopy ] 
				on: Error 
				do: [:ex | ex return: nil ].
			wc ~~ nil
				ifTrue: [
					wc repositoryGroup: nil. "glassdb Issue 220"
					assoc value do: [:location | 
						wc repositoryGroup addRepository: (self mCHttpRepositoryClass location: location user: '' password: '') ]]].
		self metacelloGemStonePlatformClass initialize.
		System commitTransaction ifFalse: [ ^self error: 'commit failed' ] ].