streaming
atEnd
	nestedStreams == nil
		ifTrue: [^peekChar == nil and: [stream atEnd]].
	^stream atEnd
		ifTrue: [
			self popNestingLevel.
			self atEnd]
		ifFalse: [false]