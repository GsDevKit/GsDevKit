install script
goferPackageVersionsFor: aVersionString

	^self filesToLoadForVersion: (ConfigurationOfGofer project version: aVersionString)
