stream creation
newFileNamed: localFileName
	"Create a new file with the given name in this directory."

	| fullName file |
	fullName := self fullNameFor: localFileName.
	(GsFile _exists: fullName onClient: self onClient)
		ifTrue: [ ^self error: 'File named: ', localFileName printString, ' already exists.' ].
	file := BinaryOrTextFile open: fullName mode: 'w+b' onClient: self onClient.
	file == nil ifTrue: [ ^self error: 'Could not create file named: ', localFileName printString ].
	^file