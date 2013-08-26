as yet unclassified
diffyFixtures
	^ #( 'Package-author.123(origAuthor.122)' 'Package-origAuthor.122'
		 'Package-author.123(origAuthor.122).mcd' 'Package-origAuthor.122.mcd'
		 'Package-author.123(122)' 'Package-author.122'
		 'Package-author.123(@origPackage-origAuthor.122)' 'origPackage-origAuthor.122'
	 ) collect:
		[ : each | each asMCVersionName ]