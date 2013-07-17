searching
packageOfSystemCategory: aSystemCategory
	^ self packageOfSystemCategory: aSystemCategory ifNone: [ self noPackageFound ]