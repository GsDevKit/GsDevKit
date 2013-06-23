searching
firstTagNamed: aSymbol with: aBlock
	"Return the first encountered node with the specified tag that
	allows the block to evaluate to true. Pass the message on"

	| answer |

	self elementsDo: [:node |
		(answer _ node firstTagNamed: aSymbol with: aBlock) ifNotNil: [^answer]].
	^nil