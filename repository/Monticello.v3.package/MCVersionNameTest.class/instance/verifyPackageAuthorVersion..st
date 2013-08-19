as yet unclassified
verifyPackageAuthorVersion: aMCFileName 
	self
		 assert: aMCFileName versionName = 'Package-author.123';
		 assert: aMCFileName packageName = 'Package' ;
		 assert: aMCFileName versionNumber = 123 ;
		 assert: aMCFileName author = 'author' ;
		 assert: aMCFileName isValid