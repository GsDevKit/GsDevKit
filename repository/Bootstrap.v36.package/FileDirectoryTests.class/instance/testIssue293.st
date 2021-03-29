tests
testIssue293
	"http://code.google.com/p/glassdb/issues/detail?id=293"
	
	self assert: (FileDirectory default directoryNamed: '/tmp') class == (FileDirectory onClient ifTrue: [ ClientFileDirectory ] ifFalse: [ ServerFileDirectory ]).
	self assert: (ServerFileDirectory default directoryNamed: '/tmp') class == ServerFileDirectory.
	self assert: (ClientFileDirectory default directoryNamed: '/tmp') class == ClientFileDirectory.