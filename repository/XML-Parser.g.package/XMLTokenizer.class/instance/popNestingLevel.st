streaming
popNestingLevel
	self hasNestedStreams
		ifTrue: [
			self stream close.
			self stream: self nestedStreams removeLast.
			self nestedStreams size > 0
				ifFalse: [nestedStreams _ nil]]