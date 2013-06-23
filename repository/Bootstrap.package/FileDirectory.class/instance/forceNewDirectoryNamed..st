operations
forceNewDirectoryNamed: localDirectoryName
	"Ensure that the receiver and all parent directory exist. Create them if necessary"
	
	| fullName |
	self exists ifFalse: [ self containingDirectory forceNewDirectoryNamed: self localName ].
	(self directoryNamed: localDirectoryName) exists
		ifFalse: [ self createDirectory: localDirectoryName].
	^self