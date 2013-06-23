testing
isActive
	"Only install a SessionMethodTransactionBoundaryPolicy _if_ session methods are enabled and GsPackage in which the class is resideds  is installed"
	
	^GsPackagePolicy current enabled and: [ (System myUserProfile symbolList objectNamed: self class name) ~~ nil ]