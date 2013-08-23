configuration
panels
	^ (Array with: self navigationPanel with: self optionalButtonPanel with: self definitionPanel)
		reject: [:ea | ea isNil]