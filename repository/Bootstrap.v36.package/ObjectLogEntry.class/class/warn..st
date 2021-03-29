instance creation
warn: aString

	^(self new label: aString object: nil)
		priority: self warn;
		yourself