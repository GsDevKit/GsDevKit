searching
firstTagNamed: aSymbol 
	"Return the first encountered node with the specified tag.
	If it is not the receiver, pass the message on"

	(self localName == aSymbol
		or: [self tag == aSymbol])
		ifTrue: [^self].
	^super firstTagNamed: aSymbol