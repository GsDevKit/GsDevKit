filtering
defaultLimitClass
	"it's too bad that this needs to be duplicated between the ProtocolBrowser and here'"
	^ (theClass isKindOf: Metaclass)
		ifTrue: [Object class]
		ifFalse: [Object]