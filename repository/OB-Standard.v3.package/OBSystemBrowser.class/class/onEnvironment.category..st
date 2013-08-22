instance creation
onEnvironment: anEnvironment category: aSymbol
	^ self selection: (OBClassCategoryNode 
						on: aSymbol 
						inEnvironment: anEnvironment)