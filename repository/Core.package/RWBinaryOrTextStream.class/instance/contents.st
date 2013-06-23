accessing
contents

	^self isBinary
		ifTrue: [ itsCollection asByteArray ]
		ifFalse: [ itsCollection asString ]
