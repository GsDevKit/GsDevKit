instance creation
debug: aString

	^(self new label: aString object: nil)
		priority: self debug;
		yourself