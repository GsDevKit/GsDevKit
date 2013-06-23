streaming
unpeek
	peekChar
		ifNotNil: [
			self stream pushBack: (String with: peekChar).
			peekChar _ nil]