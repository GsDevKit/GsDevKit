accessing-dynamic
theClass
	| rcvrClass mClass |
	rcvrClass := self context receiver class.
	mClass := self context method inClass.
	^rcvrClass == mClass
		ifTrue: [rcvrClass]
		ifFalse: [mClass]