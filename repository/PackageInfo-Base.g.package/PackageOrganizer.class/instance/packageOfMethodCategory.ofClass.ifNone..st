searching
packageOfMethodCategory: categoryName ofClass: aClass ifNone: errorBlock
	^ self packages detect: [:ea | ea includesMethodCategory: categoryName ofClassNamed: aClass] ifNone: errorBlock
	
	