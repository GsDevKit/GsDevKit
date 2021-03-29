*bootstrap
baseOverridesBootstrapDateAndTimeANSI: known
	self
		addTo: known
		forBehavior: DateAndTimeANSI
		overrides: {
			"2.4.4"
			#'_secondsUTC:offset:' -> '_secondsUTC: aNumber offset: anObject

	seconds := aNumber.
	anObject == nil ifTrue: [
		offset := self currentTimeZone offsetAtUTC: self.
	] ifFalse: [
		offset  := anObject asSeconds.
	].
'.
			"2.3.x"
			#'_secondsUTC:offset:' -> '_secondsUTC: aNumber offset: anObject

	seconds := aNumber.
	anObject == nil ifTrue: [
		offset := TimeZoneInfo default offsetAtUTC: self.
	] ifFalse: [
		offset  := anObject asSeconds.
	].
'.
			"2.4.4"
			#'initializeAsNow' -> 'initializeAsNow

	seconds := self class secondsSince2001.
	offset := (self currentTimeZone transitionAtUTC: self) offsetFromUTC.
'.
			"2.3.x"
			#'initializeAsNow' -> 'initializeAsNow

	seconds := self class secondsSince2001.
	offset := (TimeZoneInfo default transitionAtUTC: self) offsetFromUTC.
'.
			"2.4.4"
			#'_secondsLocal:offset:' -> '_secondsLocal: aNumber offset: anObject

	seconds := aNumber.
	anObject == nil ifTrue: [
		offset := self currentTimeZone offsetAtLocal: self.
	] ifFalse: [
		offset  := anObject asSeconds.
	].
	seconds := seconds - offset.
'.
			"2.3.x"
			#'_secondsLocal:offset:' -> '_secondsLocal: aNumber offset: anObject

	seconds := aNumber.
	anObject == nil ifTrue: [
		offset := TimeZoneInfo default offsetAtLocal: self.
	] ifFalse: [
		offset  := anObject asSeconds.
	].
	seconds := seconds - offset.
'.
	}