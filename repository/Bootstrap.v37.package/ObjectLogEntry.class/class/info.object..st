instance creation
info: aString object: anObject

	^(self new label: aString object: anObject)
		priority: self info;
		yourself