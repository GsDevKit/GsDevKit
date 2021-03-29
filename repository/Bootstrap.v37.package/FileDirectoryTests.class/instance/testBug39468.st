tests
testBug39468

	| main missingDir stream |
	(main := FileDirectory default directoryNamed: self mainSubdirectoryName) exists
		ifFalse: [ FileDirectory default createDirectory: self mainSubdirectoryName ].
	self assert: main exists.
	missingDir := main directoryNamed: 'missing'.
	missingDir exists 
		ifTrue: [ missingDir recursiveDelete ].
	self deny: missingDir exists.
	stream := missingDir forceNewFileNamed: 'newFile'.
	stream nextPutAll: 'hello'.
	stream close.
	self assert: missingDir exists.
	self assert: (missingDir fileExists: 'newFile').
	missingDir recursiveDelete.