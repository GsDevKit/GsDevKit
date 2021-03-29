instance creation
transcript: aString object: anObject

	^(self new label: aString object: anObject)
		priority: self transcript;
		yourself