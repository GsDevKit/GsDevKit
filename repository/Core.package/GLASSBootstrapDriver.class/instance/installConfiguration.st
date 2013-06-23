installing
installConfiguration

	self autoCommitDuring: [
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
					wc repositoryGroup addRepository: (self mCHttpRepositoryClass location: assoc value first user: '' password: '') ]].
		System commitTransaction ifFalse: [ ^self error: 'commit failed' ] ].