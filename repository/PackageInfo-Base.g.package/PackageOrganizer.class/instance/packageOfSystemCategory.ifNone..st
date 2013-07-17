searching
packageOfSystemCategory: aSystemCategory ifNone: errorBlock
	^ self packages detect: [:ea | ea includesSystemCategory: aSystemCategory] ifNone: errorBlock