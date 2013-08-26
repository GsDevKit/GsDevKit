as yet unclassified
diffyBranchFixtures
	^ #( 'Package.branch-author.123(origAuthor.122)' 'Package.branch-origAuthor.122'
		 'Package.branch-author.123(origAuthor.122).mcd' 'Package.branch-origAuthor.122.mcd'
		 'Package.branch-author.123(122)' 'Package.branch-author.122'
		 'Package.branch-author.123(@origPackage-origAuthor.122)' 'origPackage-origAuthor.122'
		 'Package-author.123(@origPackage.branch-origAuthor.122)' 'origPackage.branch-origAuthor.122'
	 ) collect:
		[ : each | each asMCVersionName ]