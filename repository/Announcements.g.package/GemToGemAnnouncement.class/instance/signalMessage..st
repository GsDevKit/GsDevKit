accessing
signalMessage: aString

	aString size > 1023 ifTrue: [self error: 'message too large for Gem to Gem Signalling'].
	signalMessage := aString