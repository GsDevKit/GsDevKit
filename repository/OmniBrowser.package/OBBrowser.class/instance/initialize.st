initializing
initialize
	panels _ OrderedCollection new.
	announcer _ OBAnnouncer new.
	cmdFactories _ #().
	self subscribe.