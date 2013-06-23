tokenizing
nextName
	| nextChar |
	nameBuffer reset.
	self peek == $.
		ifTrue: [self malformedError: 'Character expected.'].
	[(nextChar _ self peek)
		ifNil: [self errorExpected: 'Character expected.'].
	NameDelimiters at: nextChar asciiValue] whileFalse: [
			nameBuffer nextPut: self next].
	^self fastStreamStringContents: nameBuffer
"	^nameBuffer contents"