testing
foreignExtensionCategoriesForClass: aClass
	^ aClass categoryNames select: [:cat | self isForeignClassExtension: cat]