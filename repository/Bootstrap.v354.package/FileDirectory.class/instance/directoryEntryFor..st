enumeration
directoryEntryFor: filenameOrPath
	"Answer the directory entry for the given file or path. Sorta like a poor man's stat()."
	| fName dir |
	self class splitName: filenameOrPath to:[:filePath :name |
		fName := name.
		filePath isEmpty
			ifTrue: [dir := self]
			ifFalse: [dir := self class on: filePath]].
	^dir entries detect:[:entry| entry name = fName] ifNone:[nil]