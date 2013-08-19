tests
testBranches
	| noBranch otherBranch withBranch |
	noBranch :=  'Package-author.123.mcz' asMCVersionName.
	withBranch := 'Package.branch-author.123.mcz' asMCVersionName.
	otherBranch := 'Package.other-author.123.mcz' asMCVersionName.
	self
		assert: 'Package' equals: noBranch packageName;
		assert: 'Package'  equals: withBranch packageName;
		assert: 'Package-author.123' equals: noBranch versionName;
		assert: 'Package.branch-author.123'  equals: withBranch versionName;
		assert: 'Package' equals: noBranch packageAndBranchName;
		assert: 'Package.branch' equals: withBranch packageAndBranchName;
		deny: noBranch = withBranch;
		deny: withBranch = noBranch;
		deny: withBranch = otherBranch.

