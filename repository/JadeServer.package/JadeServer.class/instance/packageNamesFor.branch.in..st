Monticello
packageNamesFor: packageName branch: branchName in: url

	| repoNode glassNode glassPackageNode ancString currentVersion workingCopy newerVersions |
	repoNode := (MCHttpRepository
    		location: url
    		user: ''
    		password: '') asNode.
	repoNode refresh.
	workingCopy := MCWorkingCopy registry at: (MCPackage named: packageName) ifAbsent: [ nil ].
	workingCopy isNil
		ifTrue: [currentVersion := 0]
		ifFalse: [ancString := workingCopy ancestry ancestorString.
				currentVersion := (ancString subStrings: $.) last asInteger].
	glassNode := repoNode packagesByName detect: [:each | each name = packageName ] ifNone: [ nil ].
	glassNode repositoryNode: repoNode. "shouldn't have to do this"
	glassPackageNode := glassNode branches detect: [:each | each name = branchName ] ifNone: [ nil ].
	glassPackageNode repositoryNode: repoNode. "shouldn't have to do this"
	newerVersions := glassPackageNode versions select: [:a | a versionNumber > currentVersion].
	^self stringFromCollection: (newerVersions collect: [:each | each name asString ])