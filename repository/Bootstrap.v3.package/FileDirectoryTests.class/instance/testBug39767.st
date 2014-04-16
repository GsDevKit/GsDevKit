tests
testBug39767

	| main missingDir fName |
	main := FileDirectory on: '/opt'.
	self assert: main exists.
	missingDir := main directoryNamed: 'missing'.
	self deny: missingDir exists.
	fName := missingDir fullNameFor: 'newFile'.
	self should: [ main fileNamed: fName ] raise: Error.
	self should: [ main createDirectory: 'missing' ] raise: Error.
	self should: [ main newFileNamed: fName ] raise: Error.
	self should: [ main readOnlyFileNamed: fName ] raise: Error.
	self should: [ main forceNewFileNamed: fName ] raise: Error.