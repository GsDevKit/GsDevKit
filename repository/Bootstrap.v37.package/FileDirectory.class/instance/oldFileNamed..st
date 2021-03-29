stream creation
oldFileNamed: localFileName
	"Open the existing file with the given name in this directory."

	| fullName |
	fullName := self fullNameFor: localFileName.
	(GsFile _exists: fullName onClient: self onClient)
		ifFalse: [ ^self error: 'File named: ', localFileName printString, ' does not exists.' ].
	^BinaryOrTextFile open: fullName mode: 'r+b' onClient: self onClient.