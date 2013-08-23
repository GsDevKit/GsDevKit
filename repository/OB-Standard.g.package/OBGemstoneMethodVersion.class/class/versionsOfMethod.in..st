accessing
versionsOfMethod: aSelector in: aClass

	^(MethodVersionHistory uniqueInstance versionsOfMethod: aSelector in: aClass)
		collect: [:record | self on: record ]