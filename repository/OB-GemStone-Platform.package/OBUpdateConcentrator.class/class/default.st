accessing
default
	"self default "
	
	| default |
	default := SessionTemps current at: #OB_UPDATE_CONTRATOR_DEFAULT otherwise: nil.
	default == nil 
		ifTrue: [ 
			default := self new.
			SessionTemps current at: #OB_UPDATE_CONTRATOR_DEFAULT put: default ].
	^default