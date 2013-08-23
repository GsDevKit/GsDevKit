instance creation
selection: anInterval source: aText inCategory: aCategory inClass: aClass
	^ self new
		setClass: aClass
		category: aCategory
		source: aText
		selection: anInterval