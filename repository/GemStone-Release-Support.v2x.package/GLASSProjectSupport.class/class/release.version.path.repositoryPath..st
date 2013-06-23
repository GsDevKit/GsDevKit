basic
release: configurationClass version: versionString path: path repositoryPath: repositoryPath
	"GLASSProjectSupport
		release: ConfigurationOfGLASS
		version: '1.0-beta.0' 
		path: '/export/foos2/users/dhenrich/2.0/g_gss64bit/fast50/gs/product/seaside/bootstrap' 
		repositoryPath: '/export/foos2/users/dhenrich/2.0/g_gss64bit/fast50/gs/product/seaside/bootstrap/repository' "

	"Useful for creating custom bootstrap directories"

	^self new 
		release: configurationClass 
		version: versionString 
		path: path 
		repositoryPath: repositoryPath 
		packingListPath: nil