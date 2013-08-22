accessing
navigationPanel
	^ panels 
		detect: [:ea | ea isNavigation] 
		ifNone: [self error: 'No navigation panel configured']