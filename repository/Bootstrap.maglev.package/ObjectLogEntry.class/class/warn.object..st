instance creation
warn: aString object: anObject

	^(self new label: aString object: anObject)
		priority: self warn;
		yourself