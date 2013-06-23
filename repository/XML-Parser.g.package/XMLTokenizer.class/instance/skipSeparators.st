streaming
skipSeparators
	| nextChar |
	[((nextChar _ self peek) == nil)
		or: [SeparatorTable at: nextChar asciiValue+1]]
		whileFalse: [self next].
	(nestedStreams == nil or: [self atEnd not])
		ifFalse: [
			self checkNestedStream.
			self skipSeparators]