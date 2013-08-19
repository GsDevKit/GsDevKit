as yet unclassified
standardFixtures
	^ #('Package-author.123' 'Package-author.123.mcz' ) collect:
		[ : each | each asMCVersionName ]