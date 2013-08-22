callbacks
selection
	^ selection ifNil: [1 to: (source ifNil: [self text size] ifNotNil: [0])]