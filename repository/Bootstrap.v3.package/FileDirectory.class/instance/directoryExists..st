operations
directoryExists: filenameOrPath
	"Answer true if a directory of the given name exists. The given name may be either a full path name or a local directory within this directory."


	| fName dir |
	self class splitName: filenameOrPath to:
		[:filePath :name |
			fName := name.
			filePath isEmpty
				ifTrue: [dir := self]
				ifFalse: [dir := self directoryNamed: filePath]].

	^dir exists and: [ dir directoryNames includes: fName ].