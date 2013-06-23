tokenizing
nextNode
	| nextChar |
	"Skip < "
	self next.
	nextChar _ self peek.
	nextChar == $! ifTrue: [
		"Skip !"
		self next.
		nextChar _ self peek.
		nextChar == $- ifTrue: [^self nextComment].
		nextChar == $[ ifTrue: [^self nextCDataOrConditional].
		^self parsingMarkup
			ifTrue: [self nextMarkupDeclaration]
			ifFalse: [self nextDocType]].
	nextChar == $? ifTrue: [^self nextPI].
	^self nextTag