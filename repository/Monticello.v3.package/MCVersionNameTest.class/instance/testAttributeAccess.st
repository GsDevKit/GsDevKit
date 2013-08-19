tests
testAttributeAccess
	self allValidFixtures do: [ : each | self verifyPackageAuthorVersion: each ]