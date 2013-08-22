navigating
repositories
	| rg |
	rg := self repositoryGroup.
	^ rg repositories collect: [:ea | | node |
		node := ea asNode.
		node repositoryGroup: rg.
		node]