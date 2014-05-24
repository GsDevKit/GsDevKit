Adding
space
"Adds a space to the output stream."

| space |
space := self isBinary
	ifTrue: [ Character space asciiValue ]
	ifFalse: [ Character space ].
self nextPut: space.