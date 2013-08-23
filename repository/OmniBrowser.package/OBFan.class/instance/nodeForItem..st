accessing
nodeForItem: aString
	^ self children
		detect: [ :child | (self parent displayStringForChild: child) = aString ]
		ifNone: [ aString ]