basic
release: configurationClass version: versionString path: path repositoryPath: repositoryPath packingListPath: packingListPath
	"GLASSProjectSupport
		release: ConfigurationOfGLASS
		version: '1.0-beta.0' 
		path: '/export/foos2/users/dhenrich/2.0/c_gss64bit_300/image' 
		repositoryPath: '/export/foos2/users/dhenrich/2.0/c_gss64bit_300/examples/monticello/repository'
		packingListPath: '/export/foos2/users/dhenrich/2.0/c_gss64bit_300/build/release/PACKING.be' "

	"Useful for updating GemStone code base"
	
	^self new
		release: configurationClass 
		version: versionString 
		path: path 
		repositoryPath: repositoryPath 
		packingListPath: packingListPath