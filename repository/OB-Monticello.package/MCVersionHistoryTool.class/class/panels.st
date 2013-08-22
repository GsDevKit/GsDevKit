configuration
panels
	^ (Array with: self navigationPanel with:  self definitionPanel)
		reject: [:ea | ea isNil]
		
	