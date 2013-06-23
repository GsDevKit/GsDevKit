searching
tagsNamed: aSymbol childrenDoAndRecurse: aOneArgumentBlock
	"Evaluate aOneArgumentBlock for all children who match and recurse"

	self elementsDo: [:each | 
		each tagsNamed: aSymbol ifReceiverDoAndRecurse: aOneArgumentBlock]