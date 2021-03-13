displaying
objectString

	^self isResumable
		ifTrue: [ '^_', super objectString ]
		ifFalse: [ super objectString ]