tokenizing dtd
nextDocType
	| declType |
	declType _ self nextLiteral.
	declType = 'DOCTYPE'
		ifTrue: [
			self startParsingMarkup.
			^self nextDocTypeDecl].
	self errorExpected: 'markup declaration, not ' , declType printString