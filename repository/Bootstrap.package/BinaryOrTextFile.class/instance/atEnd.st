Positioning
atEnd

	| ans |
	(ans := super atEnd) == nil ifTrue: [ ^true ].
	^ans