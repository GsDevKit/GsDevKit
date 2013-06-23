Accessing
mailhostName
	^mailhostName ~~ nil ifTrue: [ mailhostName ] ifFalse: [ 'mailhost' ]
