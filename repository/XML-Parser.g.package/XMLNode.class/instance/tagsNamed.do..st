searching
tagsNamed: aSymbol do: aOneArgumentBlock
	"Search for nodes with tag aSymbol. When encountered evaluate aOneArgumentBlock"

	self elementsDo: [:each | each tagsNamed: aSymbol do: aOneArgumentBlock]