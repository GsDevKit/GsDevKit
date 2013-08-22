updates
unregisterDependent: anObject

	Smalltalk at: #OBUpdateConcentrator ifPresent: [:cl | ^cl default removeAsDependent: anObject ].