tokenizing
nextEntity
	"return the next XMLnode, or nil if there are no more"

	"branch, depending on what the first character is"
	self nextWhitespace.
	self atEnd ifTrue: [self handleEndDocument. ^ nil].
	self checkAndExpandReference: (self parsingMarkup ifTrue: [#dtd] ifFalse: [#content]).
	^self peek = $<
		ifTrue: [self nextNode]
		ifFalse: [self nextPCData]