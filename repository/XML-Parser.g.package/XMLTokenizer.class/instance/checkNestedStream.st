streaming
checkNestedStream
	nestedStreams == nil
		ifFalse: [(peekChar == nil and: [self stream atEnd])
			ifTrue: [
				self popNestingLevel.
				self checkNestedStream]]