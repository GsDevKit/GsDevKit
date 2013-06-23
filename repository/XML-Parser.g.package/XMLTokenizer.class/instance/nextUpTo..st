streaming
nextUpTo: delimiter
	| resultStream nextChar |
	resultStream _ WriteStream on: (String new).
	[self atEnd or: [(nextChar _ self next) == delimiter]]
		whileFalse: [resultStream nextPut: nextChar].
	nextChar == delimiter
		ifFalse: [self parseError: 'XML no delimiting ' , delimiter printString , ' found'].
	^resultStream contents