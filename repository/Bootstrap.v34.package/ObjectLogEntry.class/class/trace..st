instance creation
trace: aString

	^(self new label: aString object: nil)
		priority: self trace;
		yourself