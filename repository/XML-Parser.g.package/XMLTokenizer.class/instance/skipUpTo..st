streaming
skipUpTo: delimiter
	| nextChar |
	self unpeek.
	[self atEnd or: [(nextChar _ self next) == delimiter]]
		whileFalse: [].
	nextChar == delimiter
		ifFalse: [self parseError: 'XML no delimiting ' , delimiter printString , ' found']