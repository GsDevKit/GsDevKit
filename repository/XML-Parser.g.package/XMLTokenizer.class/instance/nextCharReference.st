tokenizing
nextCharReference
	| base charValue |
	self next == $#
		ifFalse: [self errorExpected: 'character reference'].
	base _ self peek == $x
		ifTrue: [
			self next.
			16]
		ifFalse: [10].

	charValue _ [self readNumberBase: base] on: Error do: [:ex | self errorExpected: 'Number.'].
	(self next) == $;
		ifFalse: [self errorExpected: '";"'].
	^Character value: charValue