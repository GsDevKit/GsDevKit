tests
testDiffyNames
	self diffyFixtures, self diffyBranchFixtures pairsDo: [ :diffy :base |
		self verifyDiffy: diffy base: base ]