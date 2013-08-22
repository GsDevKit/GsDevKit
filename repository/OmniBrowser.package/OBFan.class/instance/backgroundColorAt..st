accessing
backgroundColorAt: anInteger
	^(self children at: anInteger ifAbsent: [^nil]) backgroundColor