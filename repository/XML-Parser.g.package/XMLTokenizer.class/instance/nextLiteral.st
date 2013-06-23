tokenizing
nextLiteral
	| resultStream nextChar resultString |
	resultStream _ (String new: 10) writeStream.
	((nextChar _ self peek) isLetter
		or: [nextChar == $_])
		ifFalse: [self errorExpected: 'Name literal.'].
	[nextChar _ self peek.
	(LiteralChars at: nextChar asciiValue+1)
		ifTrue: [
			nextChar == $&
				ifTrue: [
					nextChar _ self next.
					resultStream nextPut: (self peek == $#
						ifTrue: [self nextCharReference]
						ifFalse: [^resultStream contents])]
				ifFalse: [
					resultStream nextPut: self next]]
		ifFalse: [resultString _ resultStream contents.
			resultString isEmpty
				ifTrue: [self errorExpected: 'Name literal']
				ifFalse: [^resultString]]] repeat