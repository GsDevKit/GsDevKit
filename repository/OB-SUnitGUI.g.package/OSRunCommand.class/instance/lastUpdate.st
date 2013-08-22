accessing
lastUpdate

	lastUpdate == nil ifTrue: [ lastUpdate := Time totalSeconds ].
	^lastUpdate