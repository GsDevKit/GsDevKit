instance creation
onClass: aClass category: aSymbol
	^ self selection: (OBMethodCategoryNode on: aSymbol inClass: aClass)