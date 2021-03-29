running
setUp

	| main |
	super setUp.
	(main := FileDirectory default directoryNamed: self mainSubdirectoryName) exists
		ifTrue: [ main recursiveDelete ].