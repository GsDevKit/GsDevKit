searching
tagsNamed: aSymbol contentsDo: aBlock
	"Evaluate aBlock for all of the contents of the receiver.
	The receiver has no tag, so pass the message on"

	self elementsDo: [:each | each tagsNamed: aSymbol contentsDo: aBlock]