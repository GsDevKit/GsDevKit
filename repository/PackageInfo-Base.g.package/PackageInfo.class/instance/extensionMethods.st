listing
extensionMethods
	^ self externalClasses gather: [:class | self extensionMethodsForClass: class]