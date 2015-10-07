initialization
initializeOn: anObject
	text := anObject withSqueakLineEndings.
	bindings := Dictionary new.
	shoutHighlighting := false