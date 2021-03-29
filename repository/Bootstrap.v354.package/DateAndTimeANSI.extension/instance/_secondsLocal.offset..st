*bootstrap
_secondsLocal: aNumber offset: anObject

	seconds := aNumber.
	anObject == nil ifTrue: [
		offset := TimeZone current offsetAtLocal: self.
	] ifFalse: [
		offset  := anObject asSeconds.
	].
	seconds := seconds - offset.