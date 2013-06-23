install script
metacelloPackageVersionsFor: aVersionString

	^self filesToLoadForVersion: (ConfigurationOfMetacello project version: aVersionString)
