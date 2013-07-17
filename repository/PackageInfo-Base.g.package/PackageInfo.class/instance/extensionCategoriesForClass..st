testing
extensionCategoriesForClass: aClass
	^ aClass categoryNames select: [:cat | self isYourClassExtension: cat]