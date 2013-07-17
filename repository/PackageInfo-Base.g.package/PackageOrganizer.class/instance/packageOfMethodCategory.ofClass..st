searching
packageOfMethodCategory: categoryName ofClass: aClass
	^self packageOfMethodCategory: categoryName ofClass: aClass ifNone: [ self noPackageFound ]