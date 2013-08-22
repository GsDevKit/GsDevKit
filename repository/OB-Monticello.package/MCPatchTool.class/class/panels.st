configuration
panels
	^ (Array 
		with: self navigationPanel 
		with: self annotationPanel 
		with: self optionalButtonPanel 
		with: self definitionPanel )
			reject: [:ea | ea isNil]