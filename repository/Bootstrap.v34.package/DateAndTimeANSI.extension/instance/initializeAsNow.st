*bootstrap
initializeAsNow

	| transition |
	seconds := self class secondsSince2001.
	offset := (transition := TimeZone current transitionAtUTC: self) == nil
		ifTrue: [ 0 ]
		ifFalse: [ transition offsetFromUTC ].