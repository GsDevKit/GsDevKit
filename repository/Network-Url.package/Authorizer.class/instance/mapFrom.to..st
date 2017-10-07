authentication
mapFrom: aKey to: aPerson
	"Establish a mapping from a RFC 1421 key to a user."

	users == nil ifTrue: [ users := Dictionary new ].
	aPerson
	 == nil ifTrue: [ users removeKey: aKey ]
	 ifFalse: [
		users removeKey: (users keyAtValue: aPerson ifAbsent: []) ifAbsent: [].
		users at: aKey put: aPerson ]
