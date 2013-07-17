testing
includesMethod: aSymbol ofClass: aClass
	aClass ifNil: [^ false].
	^ self
		includesMethodCategory: ((aClass categoryOfSelector: aSymbol)
										ifNil: [' '])
		ofClass: aClass