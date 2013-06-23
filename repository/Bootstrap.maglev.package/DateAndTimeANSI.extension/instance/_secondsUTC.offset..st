*bootstrap
_secondsUTC: aNumber offset: anObject

	seconds := aNumber.
	anObject == nil ifTrue: [
		offset := TimeZone current offsetAtUTC: self.
	] ifFalse: [
		offset  := anObject asSeconds.
	].
