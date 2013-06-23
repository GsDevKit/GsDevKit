tokenizing dtd
nextMarkupDeclaration
	| declType |
	declType _ self nextLiteral.
	self validating
		ifFalse: [^self skipMarkupDeclaration].
	declType = 'ENTITY'
		ifTrue: [self nextEntityDeclaration]
		ifFalse: [self skipMarkupDeclaration]