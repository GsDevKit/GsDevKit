stream creation
forceNewFileNamed: localFileName
	"Open the file with the given name in this directory for writing.  If it already exists, delete it first without asking."

	| fullName dir file |
	fullName := self fullNameFor: localFileName.
	dir := self class on: (self class dirPathFor: fullName).
	dir exists ifFalse: [ dir containingDirectory forceNewDirectoryNamed: dir localName ].
	file := BinaryOrTextFile open: fullName mode: 'w+b' onClient: self onClient.
	file == nil ifTrue: [ ^self error: 'Could not create file named: ', localFileName printString ].
	^file