tokenizing
nextWhitespace
	| nextChar resultStream resultString|
	resultStream _ (String new: 10) writeStream.
	[((nextChar _ self peek) == nil)
		or: [SeparatorTable at: nextChar asciiValue+1 ifAbsent:[true]]]
		whileFalse: [resultStream nextPut: nextChar. self next].
	(nestedStreams == nil or: [self atEnd not])
		ifFalse: [self checkNestedStream.
				self nextWhitespace].
	resultString _ resultStream contents.
	resultString isEmpty ifFalse: [self handleWhitespace: resultString].