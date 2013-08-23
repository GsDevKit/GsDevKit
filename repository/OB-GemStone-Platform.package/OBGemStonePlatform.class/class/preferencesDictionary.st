preferences
preferencesDictionary

	| default |
	default := SessionTemps current at: #OB_GEMSTONE_PLATFORM_PREFERENCES otherwise: nil.
	default == nil 
		ifTrue: [ 
			default := Dictionary new.
			SessionTemps current at: #OB_GEMSTONE_PLATFORM_PREFERENCES put: default ].
	^default