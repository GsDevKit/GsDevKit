testing
fileExists: filenameOrPath
	"Answer true if a file of the given name exists. The given name may be either a full path name or a local file within this directory."
	"FileDirectory default fileExists: Smalltalk sourcesName"

	| fName dir |
	self class splitName: filenameOrPath to:
		[:filePath :name |
			fName := name.
			filePath isEmpty
				ifTrue: [dir := self]
				ifFalse: [dir := self class on: filePath]].
	^dir fileNames includes: fName