tokenizing
nextAttributeValue
	| delimiterChar attributeValueStream nextChar nextPeek referenceString entity entityValue |
	delimiterChar _ self next.
	(delimiterChar == $"
		or: [delimiterChar == $'])
		ifFalse: [self errorExpected: 'Attribute value delimiter expected.'].
	attributeValueStream _ attributeBuffer reset.
	[
	nextPeek _ nextChar _ self next.
	nextChar ifNil: [self errorExpected: 'Character expected.'].
	nextChar == $&
		ifTrue: [
			self peek == $#
				ifTrue: [
					nextPeek _ nil.
					nextChar _ self nextCharReference]
				ifFalse: [
					referenceString _ self nextLiteral.
					self next == $;
						ifFalse: [self errorExpected: ';'].
					entity _ self entity: referenceString.
					entityValue _ entity valueForContext: #content.
					(self class isCharEscape: entityValue)
						ifTrue: [
							nextPeek _ nil.
							nextChar _ entityValue]
						ifFalse: [
							entityValue _ entityValue asString.
							entityValue isEmpty
								ifTrue: [nextPeek _ nextChar _ nil]
								ifFalse: [
									self pushStream: (ReadStream on: entityValue asString).
									nextPeek _ nextChar _ self next]]]].
	nextPeek == delimiterChar]
		whileFalse: [
			nextChar ifNotNil: [attributeValueStream nextPut: nextChar]].
	^self fastStreamStringContents: attributeValueStream
"	^attributeValueStream contents"