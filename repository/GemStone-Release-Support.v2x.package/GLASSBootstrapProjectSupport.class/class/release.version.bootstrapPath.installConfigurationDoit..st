custom bootstrap
release: configurationClass version: versionString bootstrapPath: bootstrapPath installConfigurationDoit: installConfigurationDoit
	"GLASSBootstrapProjectSupport
		release: ThierryConfiguration
		version: '0.232' 
		bootstrapPath: '/export/foos2/users/dhenrich/2.0/g_gss64bit/fast50/gs/product/seaside/bootstrap'
		installConfigurationDoit: 'input ThierryConfiguration.gs' "

	"Used for generating a custom bootstrap setup"
	^self new 
		installConfigurationDoit: installConfigurationDoit;
		release: configurationClass 
			version: versionString 
			path: bootstrapPath 
			repositoryPath: bootstrapPath, '/repository' 
			packingListPath: nil
