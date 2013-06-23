searching
tagsNamed: aSymbol ifReceiverOrChildDo: aOneArgumentBlock
	"If the receiver tag equals aSymbol, evaluate aOneArgumentBlock with the receiver.
	For each of the receivers children do the same. Do not go beyond direct children"

	(self localName == aSymbol
		or: [self tag == aSymbol])
		ifTrue: [aOneArgumentBlock value: self].
	super tagsNamed: aSymbol ifReceiverDo: aOneArgumentBlock