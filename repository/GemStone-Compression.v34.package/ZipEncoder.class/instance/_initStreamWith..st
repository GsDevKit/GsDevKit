Positioning
_initStreamWith: aCollectionOrStream
	aCollectionOrStream isStream 
		ifTrue:[encodedStream := aCollectionOrStream]
		ifFalse:[	encodedStream := WriteStream on: aCollectionOrStream].
	encodedStream isBinary
		ifTrue:[
			isBinary := true.
			super _initStreamWith: (ByteArray new: 4096).
		]
		ifFalse:[
			isBinary := false.
			super _initStreamWith: (String new: 4096).
		].
	bitPosition := bitBuffer := 0.