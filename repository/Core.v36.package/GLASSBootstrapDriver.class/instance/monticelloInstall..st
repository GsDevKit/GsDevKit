monticello installing
monticelloInstall: filenames

	GsPackageLibrary installMonticelloPackagesDuring: [:globalsPackage | |repository |
		repository := (globalsPackage symbolDict at: #MCFilesystemRepository) new
			directory: ((globalsPackage symbolDict at: #FSDiskFilesystem) current 
						pathFromString: self repositoryDirectory) asReference.
		filenames do: [:mczFilename |
			GsPackageLibrary loadMCZFile: mczFilename fromRepository: repository ]].
	System commitTransaction ifFalse: [ ^self error: 'commit failed' ].