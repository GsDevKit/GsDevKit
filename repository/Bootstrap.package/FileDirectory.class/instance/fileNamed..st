stream creation
fileNamed: localFileName
	"Open the file with the given name in this directory for writing."

	| fullName |
	fullName := self fullNameFor: localFileName.
	(GsFile _exists: fullName onClient: self onClient)
		ifFalse: [ ^self error: 'File named: ', localFileName printString, ' does not exist.' ].
	^BinaryOrTextFile open: fullName mode: 'a+b' onClient: self onClient.