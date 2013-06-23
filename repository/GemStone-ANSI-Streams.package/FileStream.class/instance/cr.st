Adding
cr
"Adds a newline to the output stream."

| cr |
cr := self isBinary
	ifTrue: [ Character cr asciiValue ]
	ifFalse: [ Character cr ].
self nextPut: cr.
