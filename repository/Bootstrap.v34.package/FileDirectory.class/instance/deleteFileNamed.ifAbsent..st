operations
deleteFileNamed: localFileName ifAbsent: failBlock
	"Delete the file of the given name if it exists"
	| fullName |
	fullName := self fullNameFor: localFileName.
	GsFile _removeFile: fullName onClient: self onClient