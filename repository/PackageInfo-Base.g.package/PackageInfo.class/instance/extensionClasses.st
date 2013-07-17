listing
extensionClasses
	^ self externalClasses reject: [:class | (self extensionCategoriesForClass: class) isEmpty]