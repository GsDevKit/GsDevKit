searching
tagsNamed: aSymbol ifReceiverDoAndRecurse: aOneArgumentBlock
	"Recurse all children"

	self elementsDo: [:each | each tagsNamed: aSymbol ifReceiverDoAndRecurse: aOneArgumentBlock]