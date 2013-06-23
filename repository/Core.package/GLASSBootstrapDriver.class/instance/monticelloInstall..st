metacello installing
monticelloInstall: filenames

	| repository |
	repository := self mCServerDirectoryRepositoryClass new 
						directory: (self serverFileDirectoryClass on: self repositoryDirectory).
	filenames do: [:filename | | version |
		self log: 'Monticello load: ', filename.
		version := repository loadVersionFromFileNamed: filename.
		version load ].
	System commitTransaction ifFalse: [ ^self error: 'commit failed' ].