running
tearDown

	| main |
	super tearDown.
	(main := FileDirectory default directoryNamed: self mainSubdirectoryName) exists
		ifTrue: [ main recursiveDelete ].
