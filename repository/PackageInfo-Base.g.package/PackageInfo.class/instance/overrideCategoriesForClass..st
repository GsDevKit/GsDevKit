testing
overrideCategoriesForClass: aClass
	^ aClass categoryNames select: [:cat | self isOverrideCategory: cat]