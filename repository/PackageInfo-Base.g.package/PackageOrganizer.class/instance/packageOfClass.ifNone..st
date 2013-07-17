searching
packageOfClass: aClass ifNone: errorBlock
	^ self packages detect: [:ea | ea includesClass: aClass] ifNone: errorBlock