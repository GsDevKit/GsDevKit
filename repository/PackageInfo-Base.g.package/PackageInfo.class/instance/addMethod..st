modifying
addMethod: aMethodReference
	(self includesClass: aMethodReference class)
		ifTrue: [self addCoreMethod: aMethodReference]
		ifFalse: [self addExtensionMethod: aMethodReference]