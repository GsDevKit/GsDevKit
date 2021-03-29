operations
fileNames
	"Return a collection of names for the files (but not directories) in this directory."
	"FileDirectory default fileNames"

	^ (self entries select: [:entry | (entry dirFlag) not])
		collect: [:entry | entry name]