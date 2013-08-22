configuration
panels

	^ (Array with: self navigationPanel with: self optionalButtonPanel  with: self testResultsPanel)
		reject: [:ea | ea isNil]