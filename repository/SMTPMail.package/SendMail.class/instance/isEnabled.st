testing
isEnabled
	^self preferenceAt: #mailEnabled ifAbsent: [true]