GLASS server updating
updateGLASSServer

	| version loadedVersion |
	ConfigurationOfGLASS project updateProject.
	loadedVersion := ConfigurationOfGLASS project currentVersion.
	version := self 
		chooseVersion: [:vrsn |
			"show current version and all versions later than current version except those that 
			 are #development versions"
			loadedVersion == nil
				or: [ vrsn = loadedVersion
							or: [ vrsn blessing ~~ #development 
										and: [ vrsn > loadedVersion ]]]].
	version == #all 
		ifTrue: [ version := self chooseVersion: [:vrsn | true ] ].
	version == #descriptions
		ifTrue: [ ^self fullDescriptions ].
	version == nil ifTrue: [ ^self ].
	version load: version defaultPackageNamesToLoad.
	OBPlatform current doAutoCommit 
		ifTrue: [ OBInformRequest message: version versionString, ' loaded.' ]
		ifFalse: [ OBInformRequest message: 'Commit failed. You will need to abort and try again.' ].