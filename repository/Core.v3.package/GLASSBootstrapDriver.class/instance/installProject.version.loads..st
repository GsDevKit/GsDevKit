metacello installing
installProject: projectClassName version: versionString loads: loads

	| repositories version |
	version := (System myUserProfile symbolList objectNamed: projectClassName asSymbol) new project version: versionString.
	self useRepositoryDirectory
		ifTrue: [
			repositories := { self mCServerDirectoryRepositoryClass new 
								directory: (self serverFileDirectoryClass on: self repositoryDirectory). }.
			version repositoryOverrides: repositories ].

	loads isEmpty
		ifTrue: [ version load ]
		ifFalse: [ version load: loads ].
									
	System commitTransaction ifFalse: [ ^self error: 'commit failed' ].