operations
containingDirectory
	"Return the directory containing this directory."

	^ self class on: (self class dirPathFor: pathName)