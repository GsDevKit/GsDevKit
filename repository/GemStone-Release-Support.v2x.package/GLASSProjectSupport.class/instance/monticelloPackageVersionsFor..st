install script
monticelloPackageVersionsFor: aVersionString

	^(ConfigurationOfGsMonticello project version: aVersionString) packages collect: [:pkg | pkg file ]