instance creation
trace: aString object: anObject

	^(self new label: aString object: anObject)
		priority: self trace;
		yourself