name utilities
dirPathFor: fullName 
	"Return the directory part the given name."
	self
		splitName: fullName
		to: [:dirPath :localName | ^ dirPath]