tests
testSpecies

	self
		assert: 'Package-author.123.mcz' asMCVersionName versionName isString;
		assert: 'Package-author.123' asMCVersionName versionName isString