*change-notification
classify: methodSelector under: aCategory

	(self categoryOfSelector: methodSelector) = aCategory
		ifTrue: [ ^self ].
	(self _includesCategory: aCategory)
		ifFalse: [ self addCategory: aCategory ].
	self moveMethod: methodSelector toCategory: aCategory