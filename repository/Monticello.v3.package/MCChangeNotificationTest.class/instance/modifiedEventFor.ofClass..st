events
modifiedEventFor: aSelector ofClass: aClass

	^ MethodModifiedAnnouncement 
		itemClass: aClass 
		method: (aClass compiledMethodAt: aSelector)
		oldCategory: (aClass categoryOfSelector: aSelector)
