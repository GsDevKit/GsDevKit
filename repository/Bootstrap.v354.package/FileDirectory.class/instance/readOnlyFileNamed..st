stream creation
readOnlyFileNamed: localFileName
	"Open the existing file with the given name in this directory for read-only access."

	| fullName |
	fullName := self fullNameFor: localFileName.
	(GsFile _exists: fullName onClient: self onClient)
		ifFalse: [ ^self error: 'File named: ', localFileName printString, ' does not exist.' ].
	^BinaryOrTextFile open: fullName mode: 'rb' onClient: self onClient.