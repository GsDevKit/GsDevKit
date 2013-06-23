tokenizing
nextEntityValue
	| delimiterChar entityValueStream nextChar nextPeek referenceString entity entityValue |
	delimiterChar _ self next.
	(delimiterChar == $"
		or: [delimiterChar == $'])
		ifFalse: [self errorExpected: 'Entity value delimiter expected.'].

	entityValueStream _ WriteStream on: (String new).
	[
	nextPeek _ nextChar _ self peek.
	nextChar ifNil: [self errorExpected: 'Character expected.'].
	nextChar == $&
		ifTrue: [
			self next.
			self peek == $#
				ifTrue: [
					nextPeek _ nil.
					nextChar _ self nextCharReference]
				ifFalse: [
					referenceString _ self nextLiteral.
					self next == $;
						ifFalse: [self errorExpected: ';'].
					entity _ self entity: referenceString.
					entityValue _ entity valueForContext: #entityValue.
					self pushStream: (ReadStream on: entityValue asString).
					nextPeek _ nextChar _ self next]]
		ifFalse: [
			nextChar == $%
				ifTrue: [
					self skipSeparators.
					referenceString _ self nextLiteral.
					nextChar _ self handleEntity: referenceString in: #entityValue.
					nextPeek _ nextChar _ self next]
				ifFalse: [self next]].
	nextPeek == delimiterChar]
		whileFalse: [
			nextChar ifNotNil: [entityValueStream nextPut: nextChar]].
	^entityValueStream contents