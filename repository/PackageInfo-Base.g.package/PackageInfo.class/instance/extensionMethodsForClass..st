testing
extensionMethodsForClass: aClass
	^ (self extensionCategoriesForClass: aClass)
		gather: [:cat | self methodsInCategory: cat ofClass: aClass ]