install script
corePackageVersionsFor: aVersionString

	^(ConfigurationOfGsCore project version: aVersionString) packages collect: [:pkg | pkg file ]
