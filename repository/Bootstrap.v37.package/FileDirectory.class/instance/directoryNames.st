operations
directoryNames
	"Return a collection of names for the subdirectories of this directory."
	"FileDirectory default directoryNames"

	^ (self entries select: [:entry | entry dirFlag])
		collect: [:entry | entry name]