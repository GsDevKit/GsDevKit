callbacks
text: aText
	organizer changeFromString: aText asString.
	OBAnnouncer current announce: OBRefreshRequired.
	^ true