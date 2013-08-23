menu messages
debugIt: aString in: aContext

	aString isEmpty ifTrue: [ ^self ].
	aString debugItIn: aContext