searching
firstTagNamed: aSymbol with: aBlock
	"Return the first encountered node with the specified tag that allows
	the block to evaluate to true. Pass the message on"

	((self localName == aSymbol
		or: [self tag == aSymbol])
 		and: [aBlock value: self])
		ifTrue: [^self].
	^super firstTagNamed: aSymbol with: aBlock.