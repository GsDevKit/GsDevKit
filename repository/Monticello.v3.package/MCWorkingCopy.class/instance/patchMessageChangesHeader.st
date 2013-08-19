operations
patchMessageChangesHeader
	^ String streamContents: [:stream |
		stream 
			nextPutAll: ancestry summary; cr;
			nextPutAll: 'Added, Modified, Deleted'.
		self ancestors ifNotEmpty: [
			stream nextPutAll: ' vs. '; nextPutAll: self ancestors first name].
		stream nextPut: $:].