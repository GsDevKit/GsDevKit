naming
packageName
	^ packageName ifNil: [packageName _ self categoryName]