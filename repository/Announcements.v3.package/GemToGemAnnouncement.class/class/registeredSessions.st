accessing
registeredSessions
	
	RegisteredSessions == nil ifTrue: [ RegisteredSessions := RcIdentityBag new: 100 ].
	^RegisteredSessions