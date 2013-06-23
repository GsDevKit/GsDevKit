searching
tagsNamed: aSymbol ifReceiverOrChildDo: aOneArgumentBlock
	"Recurse all children"

	self elementsDo: [:each | each tagsNamed: aSymbol ifReceiverDo: aOneArgumentBlock]