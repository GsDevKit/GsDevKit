Adding
lf
"Adds a linefeed to the output stream."

| lf |
lf := self isBinary
	ifTrue: [ Character lf asciiValue ]
	ifFalse: [ Character lf ].
self nextPut: lf.