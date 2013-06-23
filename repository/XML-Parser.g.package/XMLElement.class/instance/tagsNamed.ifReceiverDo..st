searching
tagsNamed: aSymbol ifReceiverDo: aOneArgumentBlock
	"If the receiver tag equals aSymbol, evaluate aOneArgumentBlock with the receiver"

	(self localName == aSymbol
		or: [self tag == aSymbol])
		ifTrue: [aOneArgumentBlock value: self]