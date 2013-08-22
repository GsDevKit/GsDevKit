configuration
panels
	^ {self mercuryPanel. self navigationPanel. self annotationPanel. self optionalButtonPanel. self definitionPanel} reject: [:ea | ea isNil]