testing
usesFileNamed: aFileName
	"Do I require aFileName? That is, do I care if it's clobbered?"
	^(self fileDirectoryClass default fullNameFor: externalFileName) = (self fileDirectoryClass default fullNameFor: aFileName)