testing
includesClassNamed: aClassName
	^ self includesSystemCategory: ((SystemOrganizer new categoryOfElement: aClassName) ifNil: [^false])