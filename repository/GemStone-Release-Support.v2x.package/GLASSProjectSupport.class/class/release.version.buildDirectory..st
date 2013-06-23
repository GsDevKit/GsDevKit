gemstone release
release: configurationClass version: versionString buildDirectory: buildDirectory
	"GLASSProjectSupport
		release: ConfigurationOfGLASS
		version: '1.0-beta.0' 
		buildDirectory: '/export/foos2/users/dhenrich/2.0/c_gss64bit_300' "
	
	"Used for updating a development checkout ... after this, run smoke tests and check in changes"

	^self new 
		release: configurationClass 
		version: versionString 
		path: buildDirectory, '/image' 
		repositoryPath: buildDirectory, '/examples/monticello/repository'
		packingListPath: buildDirectory, '/build/release/PACKING.be'