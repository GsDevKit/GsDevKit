as yet unclassified
testDirectoryNamedWithAbsolutePath
	self assert: (FileDirectory root directoryNamed: 'tmp') pathName = '/tmp'.
	self assert: (FileDirectory default directoryNamed: '/tmp') pathName = '/tmp'
