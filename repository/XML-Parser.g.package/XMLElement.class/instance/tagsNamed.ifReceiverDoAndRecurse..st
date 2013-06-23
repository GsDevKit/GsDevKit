searching
tagsNamed: aSymbol ifReceiverDoAndRecurse: aOneArgumentBlock
	"If the receiver tag equals aSymbol, evaluate aOneArgumentBlock
	with the receiver. Then recurse through all the children"

	(self localName == aSymbol
		or: [self tag == aSymbol])
		ifTrue: [aOneArgumentBlock value: self].
	super tagsNamed: aSymbol ifReceiverDoAndRecurse: aOneArgumentBlock