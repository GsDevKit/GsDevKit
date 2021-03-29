instance creation
info: aString

	^(self new label: aString object: nil)
		priority: self info;
		yourself