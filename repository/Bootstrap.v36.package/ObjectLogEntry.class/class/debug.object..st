instance creation
debug: aString object: anObject

	^(self new label: aString object: anObject)
		priority: self debug;
		yourself