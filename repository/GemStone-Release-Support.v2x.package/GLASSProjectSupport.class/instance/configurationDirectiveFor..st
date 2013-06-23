bootstrap packages
configurationDirectiveFor: project
	| mcLoader fetchingSpecLoader policy |
	mcLoader := project loaderClass on: nil.
	policy := MetacelloLoaderPolicy new.
	policy ignoreImage: true.
	mcLoader loaderPolicy: policy.
	MCWorkingCopy
		managersForClass: project configuration class
		do: [:mgr | | pkg |
			pkg := project packageSpec.
			mgr repositoryGroup repositories do: [:repo | pkg repositories repository: (repo asRepositorySpecFor: project) ].
			pkg name: mgr packageName.
			fetchingSpecLoader := mcLoader fetchingSpecLoader.
			fetchingSpecLoader
				linearLoadPackageSpecs: (Array with: pkg) 
				repositories: (fetchingSpecLoader repositoriesFrom: pkg repositorySpecs).
			^fetchingSpecLoader loadDirective ].
	^nil