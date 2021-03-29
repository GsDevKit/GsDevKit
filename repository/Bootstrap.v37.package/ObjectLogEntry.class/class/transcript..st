instance creation
transcript: aString

	^(self new label: aString object: nil)
		priority: self transcript;
		yourself