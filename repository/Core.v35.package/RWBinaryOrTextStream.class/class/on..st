Instance Creation
on: aCollection

	| new |
	new := super on: aCollection asString.
	aCollection class = ByteArray
		ifTrue: [ new binary ].
	^new