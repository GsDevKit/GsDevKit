callbacks
perform: aSelector orSendTo: anObject
	| receiver |
	receiver  _ (self respondsTo: aSelector) ifTrue: [self] ifFalse: [anObject].
	receiver perform: aSelector