tokenizing
nextPCData
	| resultStream nextChar referenceString entity entityValue nextPeek |
	resultStream _ (String new: 10) writeStream.
	self validating
		ifFalse: [
			[self peek == $<]
				whileFalse: [resultStream nextPut: self next].
			^self handlePCData: resultStream contents].

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
									nextPeek _ nextChar _ self peek]]]]
		ifFalse: [nextPeek == $< ifFalse: [self next]].
	nextPeek == $<]
		whileFalse: [
			nextChar ifNotNil: [resultStream nextPut: nextChar]].
	self handlePCData: resultStream contents