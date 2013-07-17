testing
foreignExtensionMethodsForClass: aClass
	^ (self foreignExtensionCategoriesForClass: aClass)
		gather: [:cat | (aClass selectorsIn: cat)
						  collect: [:sel | self referenceForMethod: sel ofClass: aClass]]