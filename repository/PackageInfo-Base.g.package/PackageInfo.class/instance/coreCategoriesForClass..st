testing
coreCategoriesForClass: aClass
	^ aClass categoryNames select: [:cat | (self isForeignClassExtension: cat) not]