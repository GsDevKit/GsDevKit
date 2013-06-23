searching
firstTagNamed: aSymbol 
	"Return the first encountered node with the specified tag. Pass the message on"

	| answer |

	self elementsDo: [:node | (answer _ node firstTagNamed: aSymbol) ifNotNil: [^answer]].
	^nil