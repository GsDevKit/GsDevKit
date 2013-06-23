searching
tagsNamed: aSymbol childrenDo: aOneArgumentBlock
	"Evaluate aOneArgumentBlock for all children who match"

	self elementsDo: [:each | 
		each tagsNamed: aSymbol ifReceiverDo: aOneArgumentBlock]