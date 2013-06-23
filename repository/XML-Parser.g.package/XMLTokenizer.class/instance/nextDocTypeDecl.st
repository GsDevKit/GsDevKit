tokenizing dtd
nextDocTypeDecl
	| nextChar |
	self skipSeparators.
	self nextLiteral.
	self skipSeparators.
	self peek == $[
		ifFalse: [[nextChar _ self peek.
				nextChar == $> or: [nextChar == $[ ]] whileFalse: [self next]].
	self peek == $[
		ifTrue: [
			self next.
			[self skipSeparators.
			self peek == $]] whileFalse: [
				self checkAndExpandReference: #dtd.
				self nextNode].
			self next == $] 
				ifFalse: [self errorExpected: ']' ]].
	self skipSeparators.
	self next == $>
		ifFalse: [self errorExpected: '>' ].

	self endParsingMarkup